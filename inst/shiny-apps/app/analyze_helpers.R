# Analyze/QC Explorer tab helper functions (P10-12: sourced by the merged
# app's app.R alongside setup_helpers.R -- kept as two separate files, not
# combined into one, since both predate the merge and each stays independently
# useful/testable on its own; the only real overlap is blank_to_null()/
# null_to_blank(), deliberately duplicated rather than shared -- see that
# pair's own comment below).
#
# Kept separate from app.R and free of Shiny-specific code (no reactives, no
# input/output objects) so load_qcsummary_table() can be sourced and called
# directly against a real output directory outside of a running Shiny
# session, for testing or scripting -- same pattern as the Setup tab's
# setup_helpers.R/build_dry_run_preview().

# ---------------------------------------------------------------------------
# Windows MAX_PATH workaround for readr file reads
# ---------------------------------------------------------------------------
#
# windows_long_path: opt a path into Windows' extended-length path API (the
# "\\?\" prefix) so a downstream file read doesn't hit the classic 260-
# character MAX_PATH limit.
#
# NOTE: this function on its own is NOT the actual fix used at any read call
# site in this file -- see windows_safe_read_tsv() below (which calls this
# function internally) for why a "\\?\"-prefixed path can't just be handed
# to readr::read_tsv() directly, and for the actual, verified call every
# read call site in this file uses instead.
#
# Found in real field use, not speculatively: this app's own recursive
# directory search (discover_qcsummary_files()) combined with this package's
# nested "derivatives/eyeQuality-v1/" output convention (get_qcsummary_output_
# path(), R/eyeQualityBatch.R) and long BIDS-style filenames routinely produce
# full paths well past 260 characters once a study's raw data already sits
# several directories deep (e.g. a Box-synced study tree) -- one real batch
# output directory produced 291 files that failed to load this way. On
# Windows, readr's underlying reader (via vroom/fs) treats the file as not
# existing at all once the full path exceeds MAX_PATH, even though the file
# is completely real. file.exists()/list.files() frequently still find/list
# it in this situation (verified: list.files(recursive = TRUE) found a real
# long-path fixture file, with a "over-long path" warning, when readr could
# not), which is exactly why a file can show up as "matched" by
# discover_qcsummary_files() and then fail to actually load: the mismatch
# between what different path-handling code recognizes is the whole bug,
# not a sign the file is actually missing.
#
# The "\\?\" prefix opts a path into the Win32 API variant with no such
# limit, but only under strict conditions that make "just prepend the
# string" wrong on its own:
#   - it must be absolute and backslash-separated -- "\\?\" paths are passed
#     through with NO further parsing by the OS (no "/", no "."/".."
#     resolution), so the path handed in must already be fully resolved
#     before the prefix is applied, not resolved by the OS afterward. That's
#     why this function always routes through normalizePath() first, rather
#     than only doing so once some length threshold is crossed -- a
#     relative path that LOOKS short can still resolve to something over
#     260 characters once it's made absolute, so guessing a threshold from
#     the input string alone would just miss cases.
#   - a UNC path ("\\server\share\...") needs the distinct "\\?\UNC\
#     server\share\..." form -- a bare "\\?\" glued onto a UNC path's
#     leading "\\" is not a valid extended-length path.
#   - applying the prefix twice (e.g. if some caller already ran a path
#     through this function) must be a no-op, not a broken double-prefixed
#     string.
# It's also a deliberate no-op on any non-Windows platform: MAX_PATH and
# "\\?\" are both Windows-only concepts, and prepending this prefix on
# Linux/macOS would just corrupt an otherwise-valid path.
#
# normalizePath(..., mustWork = FALSE) is used rather than mustWork = TRUE:
# some call sites in this file (e.g. resolve_preproc_data_path()'s sibling
# file, which may not exist) legitimately need to normalize a path that
# might not exist on disk without erroring, and every caller here already
# has its own file.exists() check (or readr's own error handling) downstream
# for the "genuinely missing" case -- this function's only job is path
# string normalization, not existence checking.
#
# IMPORTANT, and NOT just a theoretical concern -- confirmed directly against
# a real >260-character nested path on the project's authoritative Windows R
# environment: normalizePath() itself has its OWN silent failure mode past
# MAX_PATH, distinct from (but easily confused with) the bug this whole
# function exists to fix. When the path is long AND its tail doesn't yet
# exist on disk (the common case here -- a file this app is *about* to
# construct/check, not one already sitting on disk), normalizePath(mustWork
# = FALSE) can come back completely unresolved: original forward slashes
# still present, no absolute-ification performed, as if it had simply
# returned its input unchanged rather than erroring or truncating (R on
# Windows appears to size normalizePath()'s underlying buffer to the classic
# MAX_PATH, and mustWork = FALSE suppresses the resulting failure into a
# silent passthrough instead of a warning/error). A shorter reproduction
# with the same nonexistent-tail shape but comfortably under 260 characters
# resolves correctly, confirming length (combined with nonexistence) is
# what triggers it, not something else about the path's shape.
#
# That means this function can NOT simply trust normalizePath()'s output
# once a path is long -- exactly the paths it exists to handle. Every path
# below is therefore ALSO run through this function's own manual
# resolution (force_backslash / is_absolute / collapse_dot_segments) after
# normalizePath(), unconditionally rather than only as a length-triggered
# fallback -- harmless (a no-op) on an already-fully-resolved absolute
# backslash path with no "."/".." segments (the case where normalizePath()
# succeeded), and load-bearing on the case where it silently didn't.
#
# path: a single character path (any length; short paths pass through
#   normalizePath()+prefixing harmlessly, so this is safe to apply
#   unconditionally rather than only above some length threshold).
#
# Returns a single character string: `path` unchanged on non-Windows
# platforms or for NULL/NA/empty input, otherwise the normalized,
# extended-length-prefixed form.
windows_long_path <- function(path) {
  if (.Platform$OS.type != "windows") {
    return(path)
  }
  if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(path)
  }

  # trimws() FIRST, unconditionally: a stray leading space before a drive
  # letter (e.g. " C:/Users/...") makes the absolute-path check below
  # misclassify a genuinely-absolute path as relative, triggering a
  # getwd()-prepending fallback that produces a garbled result -- confirmed
  # by direct reproduction, not theoretical; see R/windowsLongPath.R's
  # sibling copy of this function for the full write-up of exactly what
  # went wrong and why it looks like a "Failed to make directory ' C:'"
  # EINVAL from a downstream fs::dir_create()/similar caller.
  path <- trimws(path)
  if (!nzchar(path)) {
    return(path)
  }

  if (startsWith(path, "\\\\?\\")) {
    return(path)
  }

  resolved <- tryCatch(
    normalizePath(path, winslash = "\\", mustWork = FALSE),
    error = function(e) path
  )

  # Manual second pass -- see the header comment above on why this can't be
  # skipped just because normalizePath() already ran.
  resolved <- .windows_force_backslash(resolved)
  if (!.windows_is_absolute_path(resolved)) {
    cwd <- sub("\\\\+$", "", .windows_force_backslash(getwd()))
    resolved <- paste0(cwd, "\\", resolved)
  }
  resolved <- .windows_collapse_dot_segments(resolved)

  if (startsWith(resolved, "\\\\?\\")) {
    return(resolved)
  }

  if (startsWith(resolved, "\\\\")) {
    # UNC form: "\\server\share\..." -> "\\?\UNC\server\share\...". substring()
    # drops the leading pair of backslashes already present in `resolved`
    # (kept as literal "\\\\" in the R source below, i.e. two characters),
    # since the "\\?\UNC\" replacement below supplies its own.
    paste0("\\\\?\\UNC\\", substring(resolved, 3))
  } else {
    paste0("\\\\?\\", resolved)
  }
}

# .windows_force_backslash: replace every "/" with "\\" -- R's own path
# helpers (file.path(), and by extension list.files(full.names = TRUE),
# which is how nearly every path windows_long_path() ever sees actually
# arrives) default to "/" as their separator even on Windows, and
# normalizePath()'s winslash = "\\" argument is exactly the part of
# normalizePath() confirmed above to silently no-op on a long,
# mostly-nonexistent path -- so this has to be redone by hand rather than
# assumed already done.
.windows_force_backslash <- function(p) {
  gsub("/", "\\\\", p, fixed = TRUE)
}

# .windows_is_absolute_path: TRUE for a drive-letter ("C:\...") or UNC
# ("\\server\...") absolute Windows path. Deliberately checked AFTER
# .windows_force_backslash() has already run on the candidate -- this
# regex only recognizes the backslash form.
.windows_is_absolute_path <- function(p) {
  grepl("^[A-Za-z]:\\\\", p) || grepl("^\\\\\\\\", p)
}

# .windows_collapse_dot_segments: manually collapse "."/".." path segments,
# preserving the drive ("C:\") or UNC ("\\server\share\") root untouched.
# Needed because a "\\?\"-prefixed path is passed to Windows with NO further
# parsing -- unlike a normal path, the OS will NOT resolve "."/".." itself,
# so any such segments have to be gone before this function's caller adds
# that prefix. Also compensates for the case (see windows_long_path()'s own
# header comment) where normalizePath() silently failed to do this itself.
#
# p: an already-absolute, already-backslash-separated path (i.e. already run
#   through .windows_force_backslash() and confirmed absolute by
#   .windows_is_absolute_path()).
#
# Returns a single character string with the same root, but with every
# "."/".." segment collapsed out of the remainder.
.windows_collapse_dot_segments <- function(p) {
  if (grepl("^[A-Za-z]:\\\\", p)) {
    root <- substr(p, 1, 3)
    rest <- substr(p, 4, nchar(p))
  } else if (grepl("^\\\\\\\\", p)) {
    m <- regmatches(p, regexec("^(\\\\\\\\[^\\\\]+\\\\[^\\\\]+\\\\)(.*)$", p))[[1]]
    if (length(m) != 3) {
      # Doesn't match the expected "\\server\share\..." shape (e.g. just
      # "\\server\share" with nothing following) -- nothing to collapse.
      return(p)
    }
    root <- m[2]
    rest <- m[3]
  } else {
    # Not actually absolute (shouldn't happen given this is only ever
    # called after .windows_is_absolute_path() confirms it is) -- returned
    # unchanged rather than guessing at a root to preserve.
    return(p)
  }

  segments <- strsplit(rest, "\\\\")[[1]]
  segments <- segments[nzchar(segments)]
  stack <- character(0)
  for (seg in segments) {
    if (identical(seg, ".")) {
      next
    } else if (identical(seg, "..")) {
      if (length(stack) > 0) {
        stack <- stack[-length(stack)]
      }
    } else {
      stack <- c(stack, seg)
    }
  }
  paste0(root, paste(stack, collapse = "\\"))
}

# windows_safe_read_tsv: the actual call every long-path-vulnerable
# readr::read_tsv() call site in this file should use, in place of calling
# readr::read_tsv(path, ...) directly.
#
# IMPORTANT -- this is NOT simply "readr::read_tsv(windows_long_path(path),
# ...)", and confirming that the naive version doesn't actually work was the
# single most important thing this fix's real-long-path verification (see
# this fix's accompanying test) caught: readr (backed by vroom, which in
# turn leans on the `fs` package for its own path handling/existence checks)
# does NOT reliably honor a "\\?\"-prefixed extended-length path passed to
# it as a plain string. Confirmed directly: `fs::file_exists()` on a real,
# genuinely-existing, `windows_long_path()`-prefixed long path returns
# FALSE, and `fs::path_real()` on that same path errors with ENOENT --
# `fs` appears to internally rewrite the path's backslashes to forward
# slashes for its own cross-platform representation (fs::path_real() on
# such an input visibly comes back as "//?/C:/Users/...", not
# "\\?\C:\Users\..."), which silently destroys the "\\?\" prefix's actual
# meaning to Windows (the prefix is only recognized in its literal
# backslash form) before the path ever reaches an actual file-open call.
# The result: readr::read_tsv(windows_long_path(path), ...) still fails
# with "does not exist" on a real long path, even though the file
# demonstrably exists (confirmed via file.size() on that same string
# returning the correct, nonzero size) -- readr's own pre-open existence
# check is what's failing, not the eventual read itself.
#
# What DOES work, confirmed directly on the same real long-path fixture:
# base R's own file() connections correctly honor a "\\?\"-prefixed path
# (readLines()/read.delim() succeed on it directly), and readr::read_tsv()
# accepts an already-open connection in place of a path string. So the fix
# is to have BASE R (not readr/vroom/fs) do the actual file opening -- this
# function opens a binary connection itself via windows_long_path(), and
# hands readr::read_tsv() that connection rather than the path string,
# sidestepping fs's mangling entirely since fs's path logic is never
# consulted for a connection object.
#
# A plain, unprefixed readr::read_tsv(path, ...) is used on non-Windows
# platforms (windows_long_path() is a no-op there, so opening a redundant
# connection ourselves would add no value, just another thing that could
# behave slightly differently from calling readr directly).
#
# The connection is explicitly closed on exit (readr::read_tsv() does NOT
# close a connection it did not open itself -- confirmed directly: the
# connection remains isOpen() == TRUE after a successful read) -- via
# on.exit() rather than a plain trailing close(), so it's still released on
# an error partway through the read (avoiding a leaked, potentially
# locking, open file handle).
#
# Opening the connection itself is wrapped so that a failure to open (e.g.
# a missing file) raises a clear, specific error -- file()'s own default
# failure behavior is to first emit a WARNING with the actual OS reason
# (e.g. "No such file or directory") and only then raise a generic "cannot
# open the connection" error whose message alone loses that detail; the
# warning handler below intercepts at the point the specific reason is
# still available and re-raises it as the error instead.
#
# path: a single file path (any length).
# ...: forwarded to readr::read_tsv() (e.g. show_col_types, progress).
#
# Returns whatever readr::read_tsv() returns (or raises whatever error it,
# or the connection-open step, raises) -- a drop-in replacement for calling
# readr::read_tsv(path, ...) directly at every call site in this file.
windows_safe_read_tsv <- function(path, ...) {
  if (.Platform$OS.type != "windows") {
    return(readr::read_tsv(path, ...))
  }

  con <- tryCatch(
    file(windows_long_path(path), open = "rb"),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  )
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  readr::read_tsv(con, ...)
}

# windows_safe_write_tsv: the write-side counterpart to
# windows_safe_read_tsv() -- used by save_notes_table() below, the one write
# call site in this file that writes to a filesystem-discovered directory
# path (as opposed to a path this file itself just constructed and knows to
# be short).
#
# This was explicitly NOT assumed to need the same fix as the read side --
# confirmed directly, rather than guessed, that it does. Tested
# readr::write_tsv() against a real >260-character destination path (a
# notes.tsv sitting at the root of a deeply-nested long output directory,
# the exact scenario save_notes_table() runs in for real study data): it
# fails with the identical "path too long" error `fs` raises on the read
# side (see windows_safe_read_tsv()'s header comment on `fs`'s role here) --
# the write path goes through the same `fs`-backed path handling as the read
# path, so it was never actually a question of IF this needed fixing, only
# of confirming that rather than assuming it from the read-side finding
# alone.
#
# path: a single file path (any length).
# ...: forwarded to readr::write_tsv() (e.g. any future formatting args).
#
# Returns whatever readr::write_tsv() returns (invisibly, matching
# readr::write_tsv()'s own return contract).
windows_safe_write_tsv <- function(df, path, ...) {
  if (.Platform$OS.type != "windows") {
    return(readr::write_tsv(df, path, ...))
  }

  con <- tryCatch(
    file(windows_long_path(path), open = "wb"),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  )
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  readr::write_tsv(df, con, ...)
}

# windows_safe_file_exists: a file.exists() replacement for the gates that
# guard several of this file's windows_safe_read_tsv() call sites (e.g.
# load_plot_data()'s "does the sibling preproc.tsv exist" check below).
#
# NOT redundant with windows_safe_read_tsv() -- confirmed directly, on the
# same real long-path fixture used to verify that function, that base R's
# own file.exists() has the identical false-negative failure mode on a
# long path that readr/vroom/fs has: file.exists() returned FALSE for a
# file that demonstrably existed (confirmed via file.size() on the exact
# same string returning the correct, nonzero size, and via
# list.files(recursive = TRUE) listing it). This was checked directly
# rather than assumed reliable -- the header comment on windows_long_path()
# above notes that file.exists()/list.files() "frequently" still find a
# long-path file where readr can't, which is true of list.files() in every
# case tested here, but file.exists() itself turned out to be just as
# unreliable as readr for a long path once actually tested against one, on
# this project's authoritative Windows R environment. Left unfixed, every
# gate below would keep reporting a real file as "missing" before
# windows_safe_read_tsv() ever got a chance to run -- i.e. those three read
# call sites would still appear broken to a user even after
# windows_safe_read_tsv() itself was fixed, since the gate in front of each
# would still say "not found" first.
#
# Reuses the exact same mechanism windows_safe_read_tsv() itself relies on
# (a windows_long_path()-prefixed base R connection, which IS confirmed
# reliable for long paths) rather than inventing a second, independent
# long-path workaround: opening the file for read and immediately closing
# it is treated as the existence check itself, since that's the actual
# operation being gated on succeeding a moment later anyway.
#
# path: a single file path (any length).
#
# Returns TRUE/FALSE. On non-Windows platforms, a plain file.exists() call
# (no known equivalent issue on those platforms, and no reason to pay for
# an extra open/close round trip there).
windows_safe_file_exists <- function(path) {
  if (.Platform$OS.type != "windows") {
    return(file.exists(path))
  }

  con <- tryCatch(
    file(windows_long_path(path), open = "rb"),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(con)) {
    return(FALSE)
  }
  close(con)
  TRUE
}

# .safe_basename: a basename()-equivalent implemented as pure string
# manipulation, with NO underlying OS/file-API call -- used everywhere in
# this file that would otherwise call base R's own basename() on a
# filesystem-discovered path (derive_recording_label(), derive_batch_name(),
# and the two load_plot_data()/load_gaze_trajectory_data() error messages
# below), for exactly the same reason windows_safe_read_tsv()/
# windows_safe_file_exists() exist: a real, distinct, empirically-confirmed
# long-path failure mode, this time inside base R itself rather than
# readr/vroom/fs.
#
# Confirmed directly (not assumed) on the project's authoritative Windows R
# environment: base R's own basename() (and dirname()) raise a plain
# "path too long" error for a path around 300 characters -- well past the
# lengths this app's own real-world fixtures produce (discover_qcsummary_
# files() found a real, over-260-character file via list.files() just fine;
# calling basename() on that EXACT same string, unchanged, is what then
# errors). This appears to be R's Windows build's own internal fixed-size
# path buffer (independent of, and with a tighter effective limit than,
# either the classic Win32 MAX_PATH story this whole fix is about, or the
# "\\?\" extended-length escape windows_long_path() implements) -- i.e. a
# THIRD, distinct long-path failure point in this file's dependency chain,
# not a restatement of the readr/vroom/fs issue windows_safe_read_tsv()
# already fixes. Left unfixed, read_one_qcsummary() (which calls
# derive_recording_label()/derive_batch_name(), both of which call
# basename()) would still fail on a real long qcsummary.tsv path even after
# windows_safe_read_tsv() itself succeeds at the actual file read -- this is
# NOT a redundant fix, it was a genuinely separate crash found empirically
# while verifying read_one_qcsummary() end-to-end against a real long path,
# not by reasoning about readr/vroom in isolation.
#
# Verified to match base R's own basename() output exactly across the
# ordinary cases this file's paths can take (trailing slash, root-only,
# mixed "/"/"\\" separators, a bare filename with no directory component,
# spaces in the filename) -- this is meant as a drop-in, not an
# approximation.
#
# path: a single character path (or NA/"" -- basename()'s own edge-case
#   behavior for those is preserved: NA in, NA out; "" in, "" out).
#
# Returns a single character string: the final path component, with any
# trailing "/"/"\\" stripped first (matching basename()'s own behavior for
# a path ending in a directory separator).
.safe_basename <- function(path) {
  if (is.na(path)) {
    return(NA_character_)
  }
  path <- sub("[/\\\\]+$", "", path)
  m <- regmatches(path, regexpr("[^/\\\\]*$", path))
  if (length(m) == 0) "" else m
}

# qcsummary_filename_pattern: the shared naming convention every qcsummary
# output is written under -- see get_qcsummary_output_path()/saveFiles() in
# R/eyeQualityBatch.R and R/saveFiles.R, which construct
# "<...>_desc-<batchName>_preproc_qcsummary.tsv" (batchName present) or
# "<...>_desc-preproc_qcsummary.tsv" (batchName NULL). Note the second form has
# no underscore directly before "preproc" -- it's "desc-preproc", not
# "desc-_preproc" -- because saveFiles()'s qcsummarydesc is built as
# paste0("_desc-", if (is.null(batchName)) "" else paste0(batchName, "_"),
# "preproc_qcsummary"), so the batchName segment (and its trailing "_")
# simply isn't there when batchName is NULL. A fixed "_preproc_qcsummary.tsv"
# suffix (with a required leading underscore) therefore only matches the
# batchName-present form and silently misses every batchName == NULL output --
# the naming form eyeQuality()'s single-file (non-batch) entry point produces
# by default. This app has no a-priori batchName to filter on -- unlike the
# Setup app's count_completed_qcsummary_files(), which polls a single known
# run -- so the pattern below deliberately leaves the "_desc-<batchName>_"
# portion unanchored (matching zero or more characters between "_desc-" and
# "preproc_qcsummary.tsv$", covering both naming forms), picking up every
# batch/run found under the chosen directory.
qcsummary_filename_pattern <- "_desc-.*preproc_qcsummary\\.tsv$"

# Human-readable rendering of qcsummary_filename_pattern for diagnostic
# messages -- the regex form above is right for list.files()'s pattern
# argument, but showing its escape characters (`\\.`, `$`) verbatim to a user
# reading a diagnostic panel is just noise.
qcsummary_filename_pattern_display <- "..._desc-...preproc_qcsummary.tsv"

# discover_qcsummary_files: find every qcsummary.tsv output under a directory,
# using qcsummary_filename_pattern. Recursive by default, since
# eyeQualityBatch()'s default outputDir = NULL writes each file's qcsummary
# output into a "derivatives/eyeQuality-v1/" subfolder nested under that
# file's own directory (see get_qcsummary_output_path()), not flat at the top
# level of a BIDS-like directory tree.
#
# Returns a character vector of full paths (possibly empty), sorted for
# stable, deterministic ordering across calls.
discover_qcsummary_files <- function(directory, recursive = TRUE) {
  if (!is.character(directory) || length(directory) != 1 || is.na(directory) || !nzchar(directory)) {
    stop("discover_qcsummary_files: 'directory' must be a non-empty single path")
  }
  if (!dir.exists(directory)) {
    stop("discover_qcsummary_files: directory does not exist: ", directory)
  }

  sort(list.files(
    directory,
    pattern = qcsummary_filename_pattern,
    recursive = recursive,
    full.names = TRUE
  ))
}

# derive_recording_label: strip the "_desc-<batchName>_preproc_qcsummary.tsv"
# (or "_desc-preproc_qcsummary.tsv") suffix off a qcsummary output's basename,
# leaving the BIDS-like recording identifier the file was generated from
# (e.g. "sub-01_ses-1_task-x_recording-eyetracking_physio") -- this is what
# lets a user tell participants/recordings apart in the combined table, since
# it directly reflects the original input filename rather than an opaque
# batch-internal id.
#
# Uses qcsummary_filename_pattern directly (rather than re-deriving a
# "_desc-.*" + fixed-suffix pattern locally) so this stays in lockstep with
# discover_qcsummary_files() -- anything discover_qcsummary_files() matches is
# guaranteed to have its "_desc-...preproc_qcsummary.tsv" suffix stripped
# here too, for either naming form.
#
# Returns a single character string.
derive_recording_label <- function(qcsummary_path) {
  sub(qcsummary_filename_pattern, "", .safe_basename(qcsummary_path))
}

# derive_batch_name: pull the batchName back out of a qcsummary output's
# filename (the "_desc-<batchName>_preproc_qcsummary.tsv" convention) --
# useful for telling apart multiple runs/batches over the same recordings
# that may coexist under one output directory (e.g. reprocessed with
# different parameters under a different batchName). Returns NA_character_
# for the batchName == NULL naming form ("_desc-preproc_qcsummary.tsv"),
# where there is no batch name to recover.
#
# Unlike derive_recording_label() above, this one's regex genuinely does need
# its own literal "_preproc_qcsummary\\.tsv$" (WITH the leading underscore),
# not qcsummary_filename_pattern's unanchored form -- that underscore is
# exactly what distinguishes the two naming forms here. saveFiles()'s
# qcsummarydesc only inserts a "_" between batchName and "preproc" when
# batchName is present (paste0(batchName, "_")); when batchName is NULL that
# separator never gets added, so "preproc" sits directly after "desc-" with no
# underscore ("_desc-preproc_qcsummary.tsv"). Requiring the underscore before
# "preproc" is therefore what correctly fails to match (falling through to the
# NA_character_ branch below) for the batchName-NULL form, while still
# capturing batchName correctly when it's present. Verified against real
# output from both eyeQuality() (batchName NULL) and eyeQualityBatch() (a real
# batchName) -- see the P10-01 follow-up fix for derive_recording_label(),
# which required the fix above precisely because its old regex made the
# opposite (incorrect) assumption.
derive_batch_name <- function(qcsummary_path) {
  m <- regmatches(
    .safe_basename(qcsummary_path),
    regexec("_desc-(.*)_preproc_qcsummary\\.tsv$", .safe_basename(qcsummary_path))
  )[[1]]
  if (length(m) < 2 || !nzchar(m[2])) {
    return(NA_character_)
  }
  m[2]
}

# derive_task_label: pull the BIDS "task-<label>" entity back out of a
# recording label (derive_recording_label()'s own output, e.g.
# "sub-01_ses-1_task-x_recording-eyetracking_physio") -- useful for studies
# where the same sub/ses is recorded multiple times under different tasks and
# a user wants to narrow the "Compare files" tab down to one task, or compare
# across all of them together.
#
# Unlike derive_batch_name(), which recovers its value from the qcsummary
# OUTPUT filename's own "_desc-<batchName>_" naming convention,
# derive_task_label() reads the "task-<label>" entity out of the ORIGINAL
# recording's BIDS-style name -- i.e. it operates on derive_recording_label()'s
# return value, not on a qcsummary path directly. Callers that only have a
# qcsummary path should pass derive_recording_label(path) in.
#
# Not every recording is BIDS-named with a task- entity at all (a non-BIDS
# filename, or a BIDS name that simply omits "task-"), so this returns
# NA_character_ when absent -- the same "NA is a real, expected value" contract
# derive_batch_name() documents for its own no-batchName case. Deliberately
# unanchored (matches "task-<label>" anywhere in the string, not just
# immediately after "sub-.../ses-..."), since BIDS entities may appear in any
# order once the sub-/ses- prefix is satisfied. BIDS entity labels are
# alphanumeric only (no "_" or "-"), which is what lets [A-Za-z0-9]+ stop
# cleanly at the next "_"-separated entity or the end of the string, without
# needing to anchor on a trailing "_" or "$" itself.
#
# recording_label: a single character string (typically
#   derive_recording_label()'s output). NULL, NA, or a zero-length string all
#   fall through to the NA_character_ branch below, same as a string with no
#   "task-" entity at all.
#
# Returns a single character string.
derive_task_label <- function(recording_label) {
  if (is.null(recording_label) || length(recording_label) != 1 || is.na(recording_label) || !nzchar(recording_label)) {
    return(NA_character_)
  }
  m <- regmatches(recording_label, regexec("task-([A-Za-z0-9]+)", recording_label))[[1]]
  if (length(m) < 2 || !nzchar(m[2])) {
    return(NA_character_)
  }
  m[2]
}

# build_zero_match_diagnostic: human-readable explanation for why
# discover_qcsummary_files() found nothing under a directory, in the same
# diagnostic spirit as the Setup app's zero-match handling (P7-06) -- this
# app's UI has no console for a user to read list.files()'s silence from, so
# a blank table needs an explicit reason attached instead. Distinguishes two
# cases: no .tsv files at all under the directory (likely the wrong
# directory entirely), vs. .tsv files present but none matching the
# qcsummary naming convention (likely raw/preproc files, but no completed
# batch run yet, or output written somewhere else via a custom outputDir).
#
# Returns a single character string.
build_zero_match_diagnostic <- function(directory, recursive) {
  all_tsv <- list.files(directory, pattern = "\\.tsv$", recursive = recursive, full.names = FALSE)

  scope <- if (isTRUE(recursive)) "directory (searched recursively)" else "directory (not searched recursively)"

  if (length(all_tsv) == 0) {
    sprintf(
      paste0(
        "No .tsv files of any kind were found under this %s. eyeQuality writes qcsummary ",
        "outputs matching \"%s\" under a \"derivatives/eyeQuality-v1/\" subfolder of each ",
        "processed file's directory by default (or under a custom outputDir, if one was used ",
        "when the batch was run). Double check this is the directory a batch run was pointed ",
        "at (or its outputDir), and that a batch run has completed."
      ),
      scope, qcsummary_filename_pattern_display
    )
  } else {
    sprintf(
      paste0(
        "Found %d .tsv file(s) under this %s, but none matched the qcsummary output naming ",
        "convention (\"%s\"). This usually means a batch run either hasn't completed yet, ",
        "or wrote its outputs (via a custom outputDir) somewhere other than this directory."
      ),
      length(all_tsv), scope, qcsummary_filename_pattern_display
    )
  }
}

# read_one_qcsummary: read a single qcsummary.tsv file and attach the
# identifying columns (recording, task_name, batch_name, source_file) a
# combined multi-file table needs to tell rows from different files apart.
# calculateOutputMetrics()'s output (see R/calculateOutputMetrics.R) is one
# row per qc_metric, not one row per file -- saveFiles() writes it with a
# leading "qc_metric" column plus whatever stat columns
# calculateOutputMetrics() computed (n, percent, group_mean, etc.), so each
# qcsummary.tsv already comes in a long, metric-per-row shape rather than a
# single-row summary. That shape is preserved here (no reshaping to one row
# per file), since collapsing ~12 stat columns x ~28 metrics per file into a
# single wide row per file would need an artificial pivot with no clear
# canonical column ordering, whereas the identifying columns added below are
# enough to filter/sort/compare across files while keeping every original
# qc_metric row and column intact and generic to whatever
# calculateOutputMetrics() happens to compute (including future
# adapter-specific columns, e.g. head-mounted metrics -- P10-06).
#
# Returns a data.frame, or raises an error (via readr's own parse failure)
# if the file isn't parseable as a delimited qcsummary.tsv.
read_one_qcsummary <- function(path) {
  qc <- windows_safe_read_tsv(path, show_col_types = FALSE, progress = FALSE)
  # Computed once, as a genuine scalar, before assignment -- assigning
  # derive_recording_label(path)'s result into qc$recording first and then
  # deriving task_name from qc$recording would instead hand derive_task_label()
  # an already-recycled, nrow(qc)-length vector (harmless for derive_task_label()
  # itself when nrow(qc) == 1, but silently wrong -- always NA_character_,
  # via its own length(recording_label) != 1 guard -- for every real
  # multi-metric-row qcsummary.tsv, which is every real file this app loads).
  recording_label <- derive_recording_label(path)
  qc$recording <- recording_label
  qc$task_name <- derive_task_label(recording_label)
  qc$batch_name <- derive_batch_name(path)
  qc$source_file <- path

  # Identifying columns first, then whatever calculateOutputMetrics() wrote
  # (led by "qc_metric"), so the combined table reads
  # recording/task/batch/metric left-to-right rather than burying them after
  # a dozen stat columns. task_name is placed right after recording (ahead of
  # batch_name) since it's intrinsic to the original recording's own BIDS
  # name (see derive_task_label()), whereas batch_name instead describes
  # which processing RUN produced this particular qcsummary output.
  id_cols <- c("recording", "task_name", "batch_name", "source_file")
  dplyr::relocate(qc, dplyr::all_of(id_cols), .before = 1)
}

# load_qcsummary_table: discover and load every qcsummary.tsv output under a
# directory into one combined table, for the Analyze/QC Explorer app's main
# table view.
#
# directory: top-level output directory to search (P1-06's outputDir
#   convention -- the same directory a batch run's outputDir pointed at, or
#   the raw data directory itself when outputDir was left NULL and outputs
#   landed in each file's own nested derivatives/eyeQuality-v1/ folder).
# recursive: search directory recursively (default TRUE) -- matches the
#   nested derivatives/eyeQuality-v1/ layout eyeQualityBatch()'s default
#   outputDir = NULL produces.
#
# Returns a list:
#   n_files: number of qcsummary.tsv files found
#   files: character vector of the file paths found (possibly empty)
#   table: combined data.frame (one row per qc_metric per file), or NULL if
#     n_files == 0
#   diagnostic_message: NULL, or a human-readable string explaining why zero
#     files were found (see build_zero_match_diagnostic())
#   read_errors: named character vector of per-file error messages for any
#     matched file that failed to parse (possibly empty) -- a malformed file
#     doesn't abort the whole load, it's just excluded from `table` and
#     surfaced here instead.
load_qcsummary_table <- function(directory, recursive = TRUE) {
  files <- discover_qcsummary_files(directory, recursive = recursive)

  if (length(files) == 0) {
    return(list(
      n_files = 0L,
      files = files,
      table = NULL,
      diagnostic_message = build_zero_match_diagnostic(directory, recursive),
      read_errors = character(0)
    ))
  }

  read_errors <- character(0)
  rows <- list()
  for (f in files) {
    result <- tryCatch(
      list(ok = TRUE, value = read_one_qcsummary(f)),
      error = function(e) list(ok = FALSE, value = conditionMessage(e))
    )
    if (result$ok) {
      rows[[f]] <- result$value
    } else {
      read_errors[f] <- result$value
    }
  }

  # dplyr::bind_rows() (not rbind()) deliberately: tolerates files whose
  # columns don't exactly match (e.g. different eyeQuality versions, or
  # future adapter-specific qc_metric columns -- P10-06), filling NA for any
  # column missing from a given file rather than erroring the whole load.
  combined <- if (length(rows) > 0) dplyr::bind_rows(rows) else NULL

  list(
    n_files = length(files),
    files = files,
    table = combined,
    diagnostic_message = NULL,
    read_errors = read_errors
  )
}

# ---------------------------------------------------------------------------
# P10-02: configurable QC thresholds with visual flagging
# ---------------------------------------------------------------------------

# qc_threshold_config: the fixed set of qc_metric rows this app treats as
# sensible pass/fail thresholding candidates, out of the ~32 rows
# calculateOutputMetrics() (R/calculateOutputMetrics.R) writes per file.
#
# Deliberately NOT every metric -- most of the 32 rows are descriptive
# (eye-selection source, smoothing magnitude, fixation/saccade/blink
# duration) rather than universal pass/fail signals: they characterize *how*
# the pipeline processed a recording, not whether the recording is good or
# bad, and several (e.g. blink rate, fixation duration) have no single
# "more/less is better" direction at all -- a high blink rate isn't
# inherently a QC failure the way a low percentage of valid data is. The
# three chosen here are the ones with an unambiguous, universal direction
# and a direct bearing on "is this recording usable":
#   - valid_raw_data: the most basic gate -- what fraction of all samples
#     had any raw gaze data at all (higher is better).
#   - robustness_proportion_valid_data_to_all_data: calculateOutputMetrics()
#     itself names this with a "robustness_" prefix, and it's a distinct
#     signal from valid_raw_data -- it's computed post-IVT-classification
#     (fixation + saccade + unclassified samples, i.e. excluding blinks and
#     IVT-flagged missing time, as a fraction of all samples), so it
#     captures data quality *after* accounting for blinks/classification
#     rather than raw non-NA-ness (higher is better).
#   - interpolated_LeftEye / interpolated_RightEye: the fraction of final
#     (post-eye-selection) samples that needed gap-filling via
#     interpolation -- heavy interpolation is a classic eye-tracking QC
#     concern independent of whether the underlying raw/robustness
#     percentages already look fine (lower is better). Left and right share
#     one threshold_id/UI control below since they're the same underlying
#     concern split by eye, and a QC reviewer thinks of "how much
#     interpolation" as one question, not two.
# Explicitly NOT included (with reasoning, so this isn't just an oversight):
#   - missing_raw_data_BothEyes / final_na are exact complements of
#     valid_raw_data / final_valid (percent values sum to 1 by construction
#     in calculateOutputMetrics()) -- thresholding both directions of the
#     same underlying quantity would just double the UI for no new
#     information.
#   - blinks_*, eye_select_*, smoothed_*, ivt_fixations/saccades/
#     unclassified/missing (as raw classification proportions rather than
#     the robustness rollup above), and the duration-based
#     robustness_fixation_duration row are descriptive/paradigm-dependent
#     (e.g. what counts as a "normal" blink rate or fixation duration
#     varies by task) rather than metrics with an obvious universal
#     pass/fail cutoff -- flagging them by default would be asserting a
#     QC opinion this app has no basis for.
#
# Columns:
#   threshold_id: the UI input/reactive key this row's flagging is
#     controlled by. Multiple qc_metric rows can share one threshold_id
#     (interpolated_LeftEye/RightEye above) when they represent the same
#     underlying QC question.
#   qc_metric: the exact value in the table's qc_metric column this row
#     applies to.
#   label: UI label shown next to the threshold input.
#   direction: "min" (flag when the metric's value is BELOW the threshold --
#     for higher-is-better metrics) or "max" (flag when ABOVE -- for
#     lower-is-better metrics).
#   default_percent: default threshold, as a 0-100 percentage (matching the
#     numericInput UI unit) -- converted to the table's underlying 0-1
#     fraction (see calculateOutputMetrics()'s "percent" column, which is a
#     raw ratio, not multiplied by 100) at comparison time.
qc_threshold_config <- data.frame(
  threshold_id = c("valid_pct", "robust_pct", "interp_pct", "interp_pct"),
  qc_metric = c(
    "valid_raw_data",
    "robustness_proportion_valid_data_to_all_data",
    "interpolated_LeftEye",
    "interpolated_RightEye"
  ),
  label = c(
    "Minimum % valid raw data",
    "Minimum % robust (classifiable) data",
    "Maximum % interpolated data (either eye)",
    "Maximum % interpolated data (either eye)"
  ),
  direction = c("min", "min", "max", "max"),
  default_percent = c(80, 80, 20, 20),
  stringsAsFactors = FALSE
)

# default_qc_thresholds: qc_threshold_config's default_percent values,
# collapsed to one entry per threshold_id and converted from a 0-100
# percentage to the 0-1 fraction the table's "percent" column actually uses
# -- the form compute_qc_flags() expects. Used both to seed the UI's
# numericInput default values and as the fallback when an input is
# temporarily NULL/NA (e.g. mid-typing in the UI).
#
# Returns a named list, one entry per unique threshold_id.
default_qc_thresholds <- function() {
  unique_cfg <- qc_threshold_config[!duplicated(qc_threshold_config$threshold_id), ]
  stats::setNames(as.list(unique_cfg$default_percent / 100), unique_cfg$threshold_id)
}

# compute_qc_flags: given the combined qcsummary table (one row per
# qc_metric per file, as built by load_qcsummary_table()) and a set of
# current threshold values, return a logical vector (same length and row
# order as `table`) marking which rows cross their configured threshold.
#
# Row-level, not a per-recording rollup, deliberately: the table is already
# long-format (one row per metric per file, per P10-01's design -- see
# read_one_qcsummary()'s comment on why that shape was kept rather than
# pivoted), and a QC reviewer scanning the table wants to see exactly which
# metric(s) tripped the threshold for a given recording, not just a single
# opaque pass/fail badge that would need drilling back into the long table
# to explain. Flagging the specific row(s) that crossed keeps that
# information visible without needing a second, reshaped table. A
# per-recording rollup (e.g. "does this recording have >=1 flagged metric")
# is still one line away from this vector -- see the "flagged_recordings"
# reactive in app.R, built for P10-05's export feature to consume -- so nothing
# here forecloses that view; it's just not the table's own row unit.
#
# Rows whose qc_metric isn't one of qc_threshold_config's configured metrics
# are never flagged. Rows with an NA value for the relevant column (e.g. a
# future adapter that doesn't populate "percent" for that metric -- see
# P10-06) are also never flagged, rather than propagating NA into the
# comparison and erroring/blanking DT's styling.
#
# table: a data.frame with at least "qc_metric" and "percent" columns (as
#   produced by read_one_qcsummary()/load_qcsummary_table()). NULL, 0-row,
#   or missing either column returns an all-FALSE (or length-0) vector
#   rather than erroring.
# thresholds: a named list/vector of 0-1 fractions, keyed by threshold_id
#   (see default_qc_thresholds() for the expected shape). A missing or
#   NA/NULL entry for a given threshold_id skips flagging for that
#   threshold_id's metric(s) entirely (falls through, no comparison made)
#   rather than guessing a default -- callers (app.R) are expected to have
#   already substituted default_qc_thresholds() for any blank UI input
#   before calling this.
#
# Returns a logical vector.
compute_qc_flags <- function(table, thresholds) {
  if (is.null(table)) {
    return(logical(0))
  }
  n <- nrow(table)
  if (n == 0 || !all(c("qc_metric", "percent") %in% names(table))) {
    return(rep(FALSE, n))
  }

  flagged <- rep(FALSE, n)
  for (i in seq_len(nrow(qc_threshold_config))) {
    cfg <- qc_threshold_config[i, ]
    threshold_value <- thresholds[[cfg$threshold_id]]
    if (is.null(threshold_value) || is.na(threshold_value)) {
      next
    }

    rows <- which(table$qc_metric == cfg$qc_metric)
    if (length(rows) == 0) {
      next
    }

    values <- table$percent[rows]
    crosses <- if (identical(cfg$direction, "min")) {
      !is.na(values) & values < threshold_value
    } else {
      !is.na(values) & values > threshold_value
    }
    flagged[rows] <- flagged[rows] | crosses
  }
  flagged
}

# resolve_preproc_data_path: given a qcsummary.tsv output's full path (as
# stored in the combined table's source_file column -- see
# read_one_qcsummary() above), return the full path of its sibling
# *_preproc.tsv data file: the full per-sample preprocessed data saveFiles()
# writes alongside qcsummary.tsv (see R/saveFiles.R), and the actual input
# generateEyeTrackingPlots() (R/generateEyeTrackingPlots.R) needs -- a
# per-timestamp data.frame with gazeLeftX/gazeRightX/pupilLeft/etc. and
# gazeX.preprocessed_px/gazeY.preprocessed_px columns, not
# calculateOutputMetrics()'s one-row-per-qc_metric summary table qcsummary.tsv
# itself holds.
#
# saveFiles() builds both filenames off the same "_desc-<batchName>_preproc"
# stem (its local `preprocdesc`), with qcsummary.tsv's name just appending
# "_qcsummary" onto that same stem before the extension (its local
# `qcsummarydesc` is literally `paste0(preprocdesc-equivalent, "_qcsummary")`)
# -- so the sibling preproc data file is recovered by stripping the trailing
# "_qcsummary" immediately before ".tsv". Verified against a real
# eyeQualityBatch() run's actual output directory for P10-03 (both the
# batchName-present and batchName-NULL naming forms produce this same
# stem/suffix relationship), not just inferred from reading saveFiles()'s
# source.
#
# Returns a single character string. The returned path may not exist on
# disk (e.g. someone deleted or moved it after the batch run completed) --
# callers should check file.exists() themselves; see load_plot_data() below,
# which does exactly that.
resolve_preproc_data_path <- function(qcsummary_path) {
  sub("_qcsummary\\.tsv$", ".tsv", qcsummary_path)
}

# load_plot_data: resolve a selected qc_table row's source_file (a
# qcsummary.tsv path) to its sibling preproc data file, load that file, and
# generate its diagnostic plots via generateEyeTrackingPlots() -- reused
# directly from R/generateEyeTrackingPlots.R, not reimplemented here, per
# this app's scope (P10-03).
#
# Handles two failure modes gracefully instead of letting either crash the
# app:
#   - the missing-sibling-file case (the *_preproc.tsv was deleted, moved, or
#     renamed since the batch run completed, while its *_qcsummary.tsv sat
#     untouched)
#   - any read/plot failure once the file is found (e.g. a preproc file from
#     a different adapter/geometry missing a column
#     generateEyeTrackingPlots() expects -- see P10-06)
#
# Returns a list:
#   ok: TRUE/FALSE
#   preproc_path: the resolved sibling path (always populated, even when ok
#     == FALSE, so callers can surface it in an error message)
#   data: the loaded data.frame (only when ok == TRUE)
#   plots: the list returned by generateEyeTrackingPlots() (only when ok ==
#     TRUE)
#   error: human-readable string (only when ok == FALSE)
load_plot_data <- function(qcsummary_path) {
  preproc_path <- resolve_preproc_data_path(qcsummary_path)

  if (!windows_safe_file_exists(preproc_path)) {
    return(list(
      ok = FALSE,
      preproc_path = preproc_path,
      error = sprintf(
        paste0(
          "The preprocessed data file this row's plots depend on is missing: %s. ",
          "It should sit alongside %s (the qcsummary.tsv this row was loaded from) ",
          "in the same derivatives/eyeQuality-v1/ folder -- it may have been moved, ",
          "renamed, or deleted since the batch run completed."
        ),
        preproc_path, .safe_basename(qcsummary_path)
      )
    ))
  }

  tryCatch(
    {
      data <- windows_safe_read_tsv(preproc_path, show_col_types = FALSE, progress = FALSE)
      plots <- eyeQuality::generateEyeTrackingPlots(data)
      list(ok = TRUE, preproc_path = preproc_path, data = data, plots = plots)
    },
    error = function(e) {
      list(
        ok = FALSE,
        preproc_path = preproc_path,
        error = sprintf(
          "Failed to load or plot %s: %s",
          preproc_path, conditionMessage(e)
        )
      )
    }
  )
}

# ---------------------------------------------------------------------------
# P10-07: save/load QC thresholds via the same batch_config.yaml the Setup
# app (P9-04) reads/writes -- one shared config file per study covering both
# run parameters and QC thresholds, not a second, fragmented config format.
# ---------------------------------------------------------------------------

# recognized_qc_threshold_ids: qc_threshold_config's unique threshold_id
# values -- the single source of truth for which qcThresholds keys this app
# (and R/batchConfig.R's validate_batch_config(), which sources this exact
# file via .known_qc_threshold_ids() to build the same list) recognizes. A
# thin wrapper around qc_threshold_config (defined above) so nothing here
# duplicates that definition.
recognized_qc_threshold_ids <- function() {
  unique(qc_threshold_config$threshold_id)
}

# blank_to_null / null_to_blank: the same "blank field <-> NULL" convention
# the Setup app's helpers.R (P9-04) uses for its own optional text/numeric
# fields. Duplicated here rather than shared -- each app's helpers.R is
# already a self-contained, independently sourced file (see this file's own
# header comment), and these two converters are simple enough that
# duplication carries no real drift risk.
blank_to_null <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  if (length(x) == 1 && is.na(x)) {
    return(NULL)
  }
  if (is.character(x) && length(x) == 1 && !nzchar(trimws(x))) {
    return(NULL)
  }
  x
}

null_to_blank <- function(x) {
  if (is.null(x)) "" else x
}

# qc_thresholds_to_percent: convert a named list of 0-1 fractions keyed by
# threshold_id (the shape app.R's qc_thresholds() reactive produces, and
# compute_qc_flags() consumes) to the 0-100 percentage scale
# batch_config.yaml's qcThresholds section is stored on -- the same unit
# qc_threshold_config$default_percent and the numericInput controls
# themselves use, so a saved config's numbers read back exactly what a user
# typed rather than an internal fraction.
qc_thresholds_to_percent <- function(thresholds) {
  stats::setNames(
    lapply(thresholds, function(v) if (is.null(v) || is.na(v)) NA_real_ else v * 100),
    names(thresholds)
  )
}

# filter_recognized_qc_thresholds: drop any qcThresholds entry (as read from
# a batch_config.yaml, percent-scale) whose key isn't a currently recognized
# threshold_id, or whose value isn't a sane 0-100 percentage -- e.g. a config
# hand-edited with a typo, or written by a future eyeQuality version that
# supports a QC metric this version doesn't (P10-07's forward-compatibility
# requirement).
#
# Unlike R/batchConfig.R's validate_batch_config(), which treats either
# problem as a hard error (the right behavior when a config is about to be
# programmatically relied on or explicitly re-saved as-is), the Analyze
# app's own "Load config" flow needs to tolerate this instead: a stray or
# future threshold entry in an otherwise-fine batch_config.yaml shouldn't
# block loading the rest of that file (its run parameters, carried forward
# via app.R's loaded_config_extra, or its still-recognized threshold
# entries) into the app. That's why app.R's load handler reads via
# read_batch_config(path, validate = FALSE) and calls this function itself,
# rather than relying on validate_batch_config() to have already screened
# qcThresholds.
#
# qcThresholds: NULL, or a named list/vector as read from a batch_config.yaml
#   (percent-scale, per validate_batch_config()'s convention). A config with
#   no qcThresholds section at all (every config written before this field
#   existed, or one written by the Setup app alone) reads back as NULL here
#   too, via read_batch_config()'s default-filling -- handled the same as an
#   empty list, not an error.
#
# Returns a list: kept (named list of only the recognized, sane-valued
# entries, possibly empty), dropped (character vector of the entry names
# that were dropped, possibly empty).
filter_recognized_qc_thresholds <- function(qcThresholds) {
  if (is.null(qcThresholds) || length(qcThresholds) == 0) {
    return(list(kept = list(), dropped = character(0)))
  }

  known_ids <- recognized_qc_threshold_ids()
  is_sane_percent <- function(v) {
    is.numeric(v) && length(v) == 1 && !is.na(v) && v >= 0 && v <= 100
  }
  entry_names <- names(qcThresholds)
  if (is.null(entry_names)) {
    entry_names <- rep("", length(qcThresholds))
  }
  keep_flags <- vapply(seq_along(qcThresholds), function(i) {
    nzchar(entry_names[i]) && entry_names[i] %in% known_ids && is_sane_percent(qcThresholds[[i]])
  }, logical(1))

  list(
    kept = qcThresholds[keep_flags],
    dropped = entry_names[!keep_flags]
  )
}

# ---------------------------------------------------------------------------
# P10-04: cross-file QC metric comparison view
# ---------------------------------------------------------------------------
#
# P10-03's row-click plot view answers "what does this one file's data look
# like", and the QC table itself (P10-01/P10-02) is already sortable/
# filterable/flagged, but neither makes it easy to eyeball how one qc_metric
# stacks up *across* every loaded file at a glance, or to line several
# metrics up side by side per file. The two helpers below back a new
# "Compare files" tab in app.R that does exactly that: a per-metric bar chart
# across recordings (build_qc_comparison_plot()), and a wide, one-row-per-
# file comparison table for a chosen set of metrics (build_qc_comparison_table()).
#
# Both are read-only views over the same qc_threshold_config/compute_qc_flags()
# machinery P10-02 already built -- neither reimplements threshold direction
# logic; build_qc_comparison_plot() in particular is deliberately handed the
# already-flagged table (qc_table_flagged() in app.R) rather than recomputing
# pass/fail itself, so this view can never drift from the QC table's own
# flagging.

# build_qc_comparison_plot: a horizontal bar chart of one qc_metric's value
# across every loaded recording, for spotting outlier files at a glance
# (rather than scanning the long QC table row by row). Reuses ggplot2 (already
# a package Import, and already the plotting library
# generateEyeTrackingPlots() depends on -- see R/generateEyeTrackingPlots.R)
# rather than introducing a new plotting dependency for this one view.
#
# table_flagged: the combined qcsummary table with a "qc_flag" column already
#   attached (app.R's qc_table_flagged() reactive, i.e. load_result()$table
#   run through compute_qc_flags()) -- reused directly here so bar coloring
#   always matches the QC table's own row highlighting for the same metric,
#   rather than this function re-deriving pass/fail itself.
# metric: a single qc_metric value to plot (one of table_flagged$qc_metric).
# thresholds: the current threshold values (app.R's qc_thresholds() reactive,
#   0-1 fractions keyed by threshold_id) -- used only to position the dashed
#   reference line at the metric's live threshold, when one is configured;
#   the bar coloring itself comes from table_flagged$qc_flag, not from
#   recomputing against `thresholds` here.
#
# Returns a ggplot object, or NULL if `metric` has no rows in table_flagged
# (e.g. a stale selection left over from a previous, since-reloaded table).
build_qc_comparison_plot <- function(table_flagged, metric, thresholds) {
  if (is.null(table_flagged) || is.null(metric) || !nzchar(metric)) {
    return(NULL)
  }
  rows <- table_flagged[table_flagged$qc_metric == metric, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NULL)
  }

  cfg_row <- qc_threshold_config[qc_threshold_config$qc_metric == metric, , drop = FALSE]
  has_threshold <- nrow(cfg_row) == 1

  # Status is read straight off table_flagged's own qc_flag column (see
  # header comment above) for any metric that has a configured threshold; a
  # metric with no configured threshold (most of calculateOutputMetrics()'s
  # ~32 rows -- see qc_threshold_config's own comment on why only 3 are
  # thresholdable) gets a distinct third status rather than being
  # miscategorized as "OK", since compute_qc_flags() never sets qc_flag TRUE
  # for an unconfigured metric.
  rows$comparison_status <- if (has_threshold) {
    ifelse(rows$qc_flag, "Flagged", "OK")
  } else {
    "No threshold configured"
  }
  rows$comparison_status <- factor(
    rows$comparison_status,
    levels = c("Flagged", "OK", "No threshold configured")
  )

  # bar_label: recording alone is NOT a unique file identity -- it's derived
  # by stripping the _desc-<batchName>_ segment off the filename (see
  # derive_recording_label()), so the same subject/session reprocessed under
  # two different batchNames (e.g. re-run with different parameters, or
  # genuinely repeated recordings) produces two rows with the IDENTICAL
  # recording value. Plotting x = recording alone let ggplot2::geom_col()'s
  # default position = "stack" silently sum those rows' percent values into
  # one bar -- a real correctness bug found in field testing (a "150%
  # robustness" value that's only possible as the sum of two runs, not a
  # single file's true value). Appending batch_name whenever it's genuinely
  # ambiguous (>1 distinct batch_name sharing the same recording) keeps each
  # row on its own bar and makes which run produced which bar explicit,
  # rather than just deduplicating labels cosmetically.
  ambiguous_recordings <- names(which(tapply(
    rows$batch_name, rows$recording, function(b) length(unique(b))
  ) > 1))
  rows$bar_label <- ifelse(
    rows$recording %in% ambiguous_recordings,
    paste0(rows$recording, " [", ifelse(is.na(rows$batch_name), "NA", rows$batch_name), "]"),
    rows$recording
  )

  # Sorted (not left in table row order) so outlier files land visibly at
  # either end of the chart rather than scattered through an arbitrary
  # file-discovery order -- coord_flip() below then reads the sorted axis
  # top-to-bottom, with recording labels legible instead of overlapping
  # x-axis text.
  plot <- ggplot2::ggplot(
    rows,
    ggplot2::aes(
      x = stats::reorder(bar_label, percent),
      y = percent,
      fill = comparison_status
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(
        "Flagged" = "#c0392b",
        "OK" = "#27ae60",
        "No threshold configured" = "#7f8c8d"
      ),
      drop = FALSE
    ) +
    # percent's underlying values are 0-1 fractions (see
    # calculateOutputMetrics.R), not already scaled to 0-100 -- labeled as a
    # percentage here purely for axis display, matching the numericInput
    # thresholds' own 0-100 unit.
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::labs(
      x = NULL,
      y = metric,
      fill = "QC status",
      title = paste("Across-file comparison:", metric)
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    # Legibility at real-world scale (found too small in field testing):
    # base_size above scales most text, but axis tick labels (the file/
    # recording names on the flipped y-axis -- the actual "which file is
    # this" text a reviewer reads) and the title need bumping past what
    # base_size alone gives them.
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 13),
      axis.title = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      legend.text = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14)
    )

  if (has_threshold) {
    threshold_value <- thresholds[[cfg_row$threshold_id[1]]]
    if (!is.null(threshold_value) && !is.na(threshold_value)) {
      plot <- plot + ggplot2::geom_hline(
        yintercept = threshold_value, linetype = "dashed", color = "black"
      )
    }
  }

  plot
}

# build_qc_comparison_table: reshape the combined long qcsummary table (one
# row per qc_metric per file) to wide -- one row per file, one column per
# selected qc_metric -- so a user can line several metrics up side by side
# per recording, rather than only viewing one metric at a time (the bar chart
# above) or scrolling through the long table's every metric row per file.
#
# table: the combined qcsummary table (load_result()$table -- pre- or
#   post-qc_flag, this function only reads qc_metric/percent so either works,
#   but app.R passes the plain, un-flagged table since qc_flag isn't a
#   per-metric-column concept in wide form -- see app.R's compare_table
#   render for how threshold crossing is instead shown per output column via
#   DT::formatStyle()).
# metrics: character vector of qc_metric values to include as columns.
#
# Returns a data.frame with recording/batch_name/source_file plus one column
# per (present) entry of `metrics`, or NULL if `table` is empty/NULL, or none
# of `metrics` has any matching rows.
build_qc_comparison_table <- function(table, metrics) {
  if (is.null(table) || nrow(table) == 0 || length(metrics) == 0) {
    return(NULL)
  }
  if (!all(c("recording", "batch_name", "source_file", "qc_metric", "percent") %in% names(table))) {
    return(NULL)
  }

  subset_tbl <- table[table$qc_metric %in% metrics, c("recording", "batch_name", "source_file", "qc_metric", "percent")]
  if (nrow(subset_tbl) == 0) {
    return(NULL)
  }

  # values_fn = first-value-wins rather than the default list-column
  # behavior: guards against a (theoretically possible, e.g. a hand-edited
  # qcsummary.tsv with a duplicated qc_metric row) duplicate id/metric
  # combination producing a nested list-cell DT can't render, at the cost of
  # silently keeping just the first such row -- an edge case worth not
  # crashing on rather than one this table is trying to detect/report.
  tidyr::pivot_wider(
    subset_tbl,
    id_cols = c("recording", "batch_name", "source_file"),
    names_from = "qc_metric",
    values_from = "percent",
    values_fn = function(x) x[1]
  )
}

# --- Compare files tab: batch_name (run) filter ---
#
# Field testing surfaced that the same subject/session sometimes gets
# processed more than once under different batch_name values (a re-run with
# different parameters, or a genuinely repeated recording -- see
# build_qc_comparison_plot()'s own comment on the bar-stacking bug this
# already caused). Nothing in the "Compare files" tab let a user narrow the
# view down to one batch_name/run at a time -- unlike the QC table tab, which
# already has this via DT's per-column filter = "top" search box -- so the
# two helpers below back a batch_name multi-select filter that narrows the
# table handed to build_qc_comparison_plot()/build_qc_comparison_table(),
# without either of those functions needing any batch_name-filtering
# awareness of their own.

# batch_name_none_sentinel: the literal UI choice/selected value standing in
# for the NA_character_ "no batchName" case derive_batch_name() documents as
# a real, expected value (produced by eyeQuality()'s single-file, non-batch
# naming form, "_desc-preproc_qcsummary.tsv"). selectizeInput's `choices`/
# `selected` have to be plain strings -- NA itself can't round-trip through
# them -- and a raw "NA" string in the filter dropdown reads as a data
# problem rather than the documented, legitimate no-batch-name case, hence
# this more legible display label instead.
batch_name_none_sentinel <- function() {
  "(none)"
}

# compare_batch_name_choices: every distinct batch_name value in the
# currently loaded table, sorted, with any NA_character_ entries collapsed to
# one trailing batch_name_none_sentinel() choice -- the same "sorted, NA
# handled sensibly" shape compare_metric_choices() above uses for qc_metric.
# Empty (not NULL) before any data is loaded or if the table has no
# batch_name column, so the filter's selectizeInput always gets a well-typed
# `choices` argument.
#
# table: the combined qcsummary table (load_result()$table), or NULL.
#
# Returns a character vector.
compare_batch_name_choices <- function(table) {
  if (is.null(table) || !"batch_name" %in% names(table)) {
    return(character(0))
  }
  values <- unique(table$batch_name)
  non_na_sorted <- sort(values[!is.na(values)])
  if (any(is.na(values))) {
    c(non_na_sorted, batch_name_none_sentinel())
  } else {
    non_na_sorted
  }
}

# filter_by_batch_name: narrow a combined qcsummary table down to only the
# rows whose batch_name is one of `selected` -- the actual filtering step
# behind the "Compare files" tab's batch_name selectizeInput. Honors
# batch_name_none_sentinel() in `selected` as a stand-in for keeping the
# NA_character_ rows, mirroring compare_batch_name_choices()'s own encoding
# of NA.
#
# table: the combined qcsummary table to filter (build_qc_comparison_plot()
#   and build_qc_comparison_table() are both called with this function's
#   output rather than the raw table directly, so neither of them needs any
#   batch_name-filtering logic of its own). NULL or missing a batch_name
#   column is returned unchanged -- there's nothing sensible to filter on.
# selected: character vector of batch_name values (plus
#   batch_name_none_sentinel() for "no batchName") to keep. NULL or
#   zero-length deliberately returns a 0-row subset rather than `table`
#   unchanged -- a user who has cleared every selection in the filter has
#   asked to see nothing, not everything; both downstream builder functions
#   already degrade gracefully to "no data" in that case (see their own
#   NULL-returning empty-input handling above).
#
# Returns a data.frame (a row subset of `table`).
filter_by_batch_name <- function(table, selected) {
  if (is.null(table) || !"batch_name" %in% names(table)) {
    return(table)
  }
  if (is.null(selected) || length(selected) == 0) {
    return(table[0, , drop = FALSE])
  }
  keep_none <- batch_name_none_sentinel() %in% selected
  keep_named <- setdiff(selected, batch_name_none_sentinel())
  keep <- (table$batch_name %in% keep_named) | (keep_none & is.na(table$batch_name))
  table[keep, , drop = FALSE]
}

# --- Compare files tab: task_name filter ---
#
# Mirror of the batch_name (run) filter directly above, for the analogous
# scenario of the same sub/ses recording having multiple task-<label>
# recordings (e.g. task-x and task-y under the same subject/session) that a
# user wants to narrow the "Compare files" view down to one task, or leave
# showing every task together. Composes with the batch_name filter above --
# app.R applies both in sequence -- rather than replacing it, since the two
# filters narrow on independent axes (which processing run vs. which original
# task recording).

# task_name_none_sentinel: the literal UI choice/selected value standing in
# for the NA_character_ "no task- entity" case derive_task_label() documents
# as a real, expected value (a non-BIDS filename, or a BIDS name that simply
# omits "task-"). Kept as its own function (rather than reusing
# batch_name_none_sentinel() directly) even though the display string is
# identical, since the two sentinels stand in for NA on two independently
# documented, unrelated columns -- keeping them as separate functions means a
# future change to either display string (or either column's "what does NA
# mean here" contract) doesn't have to worry about the other silently
# following along.
task_name_none_sentinel <- function() {
  "(none)"
}

# compare_task_name_choices: every distinct task_name value in the currently
# loaded table, sorted, with any NA_character_ entries collapsed to one
# trailing task_name_none_sentinel() choice -- exactly compare_batch_name_choices()'s
# own shape, applied to task_name instead of batch_name.
#
# table: the combined qcsummary table (load_result()$table), or NULL.
#
# Returns a character vector.
compare_task_name_choices <- function(table) {
  if (is.null(table) || !"task_name" %in% names(table)) {
    return(character(0))
  }
  values <- unique(table$task_name)
  non_na_sorted <- sort(values[!is.na(values)])
  if (any(is.na(values))) {
    c(non_na_sorted, task_name_none_sentinel())
  } else {
    non_na_sorted
  }
}

# filter_by_task_name: narrow a combined qcsummary table down to only the
# rows whose task_name is one of `selected` -- exactly filter_by_batch_name()'s
# own shape, applied to task_name instead of batch_name.
#
# table: the combined qcsummary table to filter. NULL or missing a task_name
#   column is returned unchanged -- there's nothing sensible to filter on.
# selected: character vector of task_name values (plus
#   task_name_none_sentinel() for "no task- entity") to keep. NULL or
#   zero-length deliberately returns a 0-row subset rather than `table`
#   unchanged, same reasoning as filter_by_batch_name()'s own `selected` doc.
#
# Returns a data.frame (a row subset of `table`).
filter_by_task_name <- function(table, selected) {
  if (is.null(table) || !"task_name" %in% names(table)) {
    return(table)
  }
  if (is.null(selected) || length(selected) == 0) {
    return(table[0, , drop = FALSE])
  }
  keep_none <- task_name_none_sentinel() %in% selected
  keep_named <- setdiff(selected, task_name_none_sentinel())
  keep <- (table$task_name %in% keep_named) | (keep_none & is.na(table$task_name))
  table[keep, , drop = FALSE]
}

# --- P10-05: export "flagged for review" file list ---
#
# build_flagged_export_table: per-recording rollup of the currently flagged
# rows in table_flagged, shaped for a triage handoff -- a colleague opening
# the exported CSV can see which recordings were flagged, by how many
# metrics, and which metrics/values tripped them, without cross-referencing
# the full long QC table. Deliberately "flagged by any configured threshold"
# rather than a single metric -- see app.R's download handler for why that's
# the more useful default for an exclusion/re-review list.
#
# Reuses table_flagged's own qc_flag column (compute_qc_flags(), the same
# source of truth every other P10-02/P10-04 view already reads) rather than
# recomputing threshold-crossing logic a third time -- this function only
# filters and reshapes rows compute_qc_flags() already flagged. It also
# doesn't duplicate app.R's own flagged_recordings() reactive (a distinct-
# recordings list with no per-metric detail, built as a P10-05 hook back in
# P10-02) -- that reactive is a fine "how many recordings" summary, but this
# function's row-per-recording-with-detail shape is what actually belongs in
# an exported file, so it's built directly off table_flagged instead of
# wrapping that narrower reactive.
#
# table_flagged: the combined qcsummary table with a "qc_flag" column already
#   attached (app.R's qc_table_flagged() reactive). NULL, 0-row, or missing
#   any of the required columns returns NULL (nothing to export).
#
# Returns a data.frame with one row per recording that has >=1 flagged
# qc_metric row, columns:
#   recording, batch_name, source_file: the same identifying columns every
#     other view in this app keys on.
#   task_name: only present when table_flagged itself already carries a
#     task_name column (read_one_qcsummary() always attaches one in real
#     production use -- see analyze_helpers.R's "Compare files tab: task_name
#     filter" section -- so in practice this is present whenever there's real
#     data to export). Deliberately NOT added to `required_cols` below the way
#     recording/batch_name/source_file are: unlike those three, a
#     hand-built/legacy table_flagged that never carried task_name at all
#     (e.g. every synthetic table this function's own tests build) is still a
#     perfectly valid, exportable table -- there's just nothing to report for
#     a column that was never there, so this degrades gracefully by omitting
#     the column entirely rather than failing the whole export or fabricating
#     NA values for it.
#   n_flagged_metrics: count of flagged qc_metric rows for that recording.
#   flagged_metrics: comma-separated qc_metric names that were flagged, so a
#     reviewer opening the CSV can see *why* without reopening this app.
#   flagged_values: comma-separated percent values, converted to the 0-100
#     scale the sidebar's numericInput thresholds already use (matching
#     qc_thresholds_to_percent()'s convention), in the same order as
#     flagged_metrics.
#   note: only present when `notes` (below) is supplied -- that recording's
#     current review note (see the "review notes" section further down), or
#     "" if it has none.
# Or NULL if table_flagged has no usable rows, or none are currently flagged.
#
# notes: NULL (default -- preserves this function's original 6-column
#   shape exactly, unchanged, for every existing caller/test that doesn't
#   pass this argument), or a data.frame shaped like empty_notes_table()'s
#   return value (recording, batch_name, note, updated_at). When supplied,
#   left-joined onto the rollup above by (recording, batch_name) so the
#   export always carries a reviewer's own explanation alongside whichever
#   metrics triggered the flag -- app.R's download handler is the only
#   caller that passes this.
build_flagged_export_table <- function(table_flagged, notes = NULL) {
  required_cols <- c("recording", "batch_name", "source_file", "qc_metric", "percent", "qc_flag")
  if (is.null(table_flagged) || nrow(table_flagged) == 0 || !all(required_cols %in% names(table_flagged))) {
    return(NULL)
  }

  # task_name rides along whenever it's actually present (see this function's
  # own `task_name` return-value doc above), but is deliberately excluded from
  # required_cols -- a table_flagged that never carried it is still a valid
  # input, not a malformed one.
  has_task_name <- "task_name" %in% names(table_flagged)
  select_cols <- c(required_cols, if (has_task_name) "task_name")
  group_cols <- c("recording", if (has_task_name) "task_name", "batch_name", "source_file")

  flagged_rows <- table_flagged[table_flagged$qc_flag, select_cols, drop = FALSE]
  if (nrow(flagged_rows) == 0) {
    return(NULL)
  }

  result <- flagged_rows %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      n_flagged_metrics = dplyr::n(),
      flagged_metrics = paste(qc_metric, collapse = ", "),
      flagged_values = paste(round(percent * 100, 1), collapse = ", "),
      .groups = "drop"
    ) %>%
    as.data.frame()

  if (!is.null(notes) && all(c("recording", "batch_name", "note") %in% names(notes))) {
    # dplyr::left_join() (not base merge()) deliberately: it treats
    # NA == NA as a match on the join key, which base merge()'s default
    # does too but only with a less obvious incomparables= dance -- correct
    # here because derive_batch_name()'s NA_character_ ("no batchName")
    # case is a real, expected join key value (see resolve_notes_path()'s
    # own header comment), not a value that should silently fail to match.
    result <- dplyr::left_join(result, notes[, c("recording", "batch_name", "note")], by = c("recording", "batch_name"))
    result$note[is.na(result$note)] <- ""
  }

  result
}

# ---------------------------------------------------------------------------
# "Gaze Explorer" tab: interactive time-range slider + AOI (area-of-interest)
# exploration for a single selected file.
#
# Distinct from the "Plots" tab (P10-03/load_plot_data() above): that tab
# renders generateEyeTrackingPlots()'s 3 fixed, server-rendered ggplot
# outputs for the whole recording, reused as-is per this app's scope (this
# task does not touch that function or that tab). This section instead backs
# a NEW view: pick a time range (freely, or snapped to a real event marker as
# a starting point) and see that range's gaze trajectory and what fraction of
# it falls inside a user-drawn AOI polygon, both updating live as the range
# changes. Closest in spirit to the old, unintegrated app_lana.R reference
# script's own "Gaze Explorer" tab (plotly heatmap + AOI-percentage modal),
# but rebuilt here as testable, Shiny-free helpers (this section) plus thin
# reactive wiring in app.R, rather than the monolithic, un-package-integrated
# original.
# ---------------------------------------------------------------------------

# resolve_events_data_path: given a qcsummary.tsv output's full path, return
# the full path of its sibling *_events.tsv file -- the event-marker data
# saveFiles() writes alongside preproc.tsv and qcsummary.tsv (see
# R/saveFiles.R).
#
# Unlike qcsummary.tsv (a SUFFIX variant of preproc.tsv -- see
# resolve_preproc_data_path()'s own comment: same stem, "_qcsummary"
# appended before ".tsv"), events.tsv is a SIBLING built off a genuinely
# different stem. R/saveFiles.R constructs both independently from the same
# batchName:
#   eventdesc    <- "_desc-<batchName>_events"   (or "_desc-events" if NULL)
#   preprocdesc  <- "_desc-<batchName>_preproc"  (or "_desc-preproc" if NULL)
# -- i.e. they share only the common "_desc-<batchName>_" prefix, then
# diverge into "events" vs. "preproc" as two entirely separate words, not one
# derived from the other by suffixing. That means the correct derivation from
# a preproc.tsv path is to swap the trailing "preproc" segment for "events"
# immediately before ".tsv" -- NOT to strip/append a suffix the way
# resolve_preproc_data_path() does for qcsummary.tsv. Verified directly
# against a real eyeQualityBatch() output directory (both the batchName-
# present and batchName-NULL naming forms): a real run's derivatives/
# folder contains, alongside each *_preproc.tsv and *_preproc_qcsummary.tsv,
# a sibling *_events.tsv whose stem is otherwise identical -- confirming the
# substitution below recovers the exact real filename, not just an inferred
# one from reading saveFiles()'s source alone.
#
# Deliberately built on top of resolve_preproc_data_path() (not a second,
# independent regex against qcsummary_path directly) so this never drifts
# from that function's own qcsummary -> preproc derivation -- there is
# exactly one place that regex lives.
#
# Returns a single character string. The returned path may not exist on disk
# -- see load_gaze_trajectory_data() below, which treats a missing/unusable
# events.tsv as "no event markers available" (a real, expected case -- see
# that function's own comment) rather than an error, unlike
# resolve_preproc_data_path()'s sibling being missing (load_plot_data()'s own
# hard failure case).
resolve_events_data_path <- function(qcsummary_path) {
  preproc_path <- resolve_preproc_data_path(qcsummary_path)
  sub("preproc\\.tsv$", "events.tsv", preproc_path)
}

# load_gaze_trajectory_data: resolve a selected qc_table row's source_file to
# its sibling preproc data file (the same file load_plot_data() itself loads
# -- see resolve_preproc_data_path()) and its sibling events data file (see
# resolve_events_data_path()), for the Gaze Explorer tab's own interactive
# trajectory/time-range/AOI view.
#
# Deliberately a NEW, independent loader rather than an extension of
# load_plot_data(): that function already has an established, tested return
# contract (ok/preproc_path/data/plots/error) consumed both by app.R's
# existing "Plots" tab and by test-runAnalyzeApp.R's own regression suite;
# widening its return shape to also carry a second sibling file (events.tsv)
# that view has no use for would couple two independently-evolving tabs'
# data needs into one loader. preproc.tsv is small enough (one row per
# sample, already read once per "Plots" tab render today) that reading it a
# second time here, rather than threading a shared cache through both call
# sites, is not worth that coupling.
#
# Unlike load_plot_data(), which treats a missing sibling preproc.tsv as a
# hard failure (ok = FALSE), this function treats an events.tsv problem as a
# DEGRADE-not-fail condition throughout (see the `events` field below):
# adapter-aware graceful degradation, since some adapters (e.g. head-mounted
# geometry types with no stimulus-marker integration) legitimately never
# produce event markers at all, and even a Tobii adapter's own events.tsv is
# frequently 0 rows whenever a given recording simply had no logged events
# (confirmed directly: a real eyeQualityBatch() run against this repo's own
# test fixtures produces exactly that -- a real, 20-column events.tsv with 0
# data rows). Neither case should block this tab's time-range/trajectory/AOI
# view, which works fine over the whole recording's free time range with no
# event-snapping offered instead -- see app.R's own gating on
# derive_event_marker_windows()'s NULL return for this same degradation.
#
# qcsummary_path: full path to a qcsummary.tsv output (as stored in the
#   combined table's source_file column).
#
# Returns a list:
#   ok: TRUE/FALSE -- FALSE only when the trajectory data itself (the
#     sibling preproc.tsv, or its required columns) can't be loaded; a
#     missing/unreadable/empty/malformed events.tsv never sets this FALSE.
#   preproc_path / events_path: the resolved sibling paths (always
#     populated, even when ok == FALSE, so callers can surface them in an
#     error message).
#   data: the loaded preproc data.frame (only when ok == TRUE).
#   events: the loaded events data.frame if a usable one was found (file
#     exists, parses, has >= 1 row, and has both "event" and
#     "recordingTimestamp_ms" columns), or NULL otherwise -- callers
#     (app.R, derive_event_marker_windows()) already treat NULL as "no
#     event markers available" uniformly, regardless of which of those
#     reasons produced it.
#   error: human-readable string (only when ok == FALSE).
load_gaze_trajectory_data <- function(qcsummary_path) {
  preproc_path <- resolve_preproc_data_path(qcsummary_path)
  events_path <- resolve_events_data_path(qcsummary_path)

  if (!windows_safe_file_exists(preproc_path)) {
    return(list(
      ok = FALSE,
      preproc_path = preproc_path,
      events_path = events_path,
      error = sprintf(
        paste0(
          "The preprocessed data file the Gaze Explorer tab depends on is missing: %s. ",
          "It should sit alongside %s (the qcsummary.tsv this row was loaded from) in the ",
          "same derivatives/eyeQuality-v1/ folder -- it may have been moved, renamed, or ",
          "deleted since the batch run completed."
        ),
        preproc_path, .safe_basename(qcsummary_path)
      )
    ))
  }

  loaded <- tryCatch(
    {
      data <- windows_safe_read_tsv(preproc_path, show_col_types = FALSE, progress = FALSE)
      # gazeX.preprocessed_px/gazeY.preprocessed_px/recordingTimestamp_ms are
      # the three columns this tab's trajectory plot and AOI test actually
      # read (build_gaze_trajectory_plot()/compute_aoi_percent() below) --
      # checked explicitly, and up front, so a mismatched/corrupted preproc
      # file fails clearly here rather than plotly silently rendering an
      # empty or nonsensical trace further downstream.
      required_cols <- c("recordingTimestamp_ms", "gazeX.preprocessed_px", "gazeY.preprocessed_px")
      missing_cols <- setdiff(required_cols, names(data))
      if (length(missing_cols) > 0) {
        stop(sprintf("missing required column(s) for the Gaze Explorer tab: %s", paste(missing_cols, collapse = ", ")))
      }
      list(ok = TRUE, data = data)
    },
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )

  if (!isTRUE(loaded$ok)) {
    return(list(
      ok = FALSE,
      preproc_path = preproc_path,
      events_path = events_path,
      error = sprintf("Failed to load %s: %s", preproc_path, loaded$error)
    ))
  }

  # Deliberately swallows every events.tsv failure mode into a plain NULL
  # (missing file, unreadable/corrupted file, or a file that IS readable but
  # has 0 rows or lacks the columns this tab needs) -- none of them should
  # ever surface as an error here, per this function's own header comment on
  # graceful degradation.
  events <- tryCatch(
    {
      if (!windows_safe_file_exists(events_path)) {
        NULL
      } else {
        ev <- windows_safe_read_tsv(events_path, show_col_types = FALSE, progress = FALSE)
        if (nrow(ev) == 0 || !all(c("event", "recordingTimestamp_ms") %in% names(ev))) {
          NULL
        } else {
          ev
        }
      }
    },
    error = function(e) NULL
  )

  list(
    ok = TRUE,
    preproc_path = preproc_path,
    events_path = events_path,
    data = loaded$data,
    events = events
  )
}

# derive_event_marker_windows: collapse a raw events data.frame (one row per
# logged event occurrence -- see ?eyeQuality-schema's "event"/"eventValue"/
# "recordingTimestamp_ms" columns, and R/getEventTimes.R for the same
# first-occurrence/last-occurrence idea this reuses) down to one row per
# DISTINCT event label, giving that label's first-to-last occurrence window
# -- the Gaze Explorer tab's "jump to event marker" preset choices.
#
# One row per distinct label (not one row per individual occurrence)
# deliberately: an event label logged once per trial (e.g. "TrialStart"
# logged N times across N trials) would otherwise produce N near-identical,
# hard-to-tell-apart dropdown entries; collapsing to first-to-last per label
# instead gives a single, immediately useful "jump to every occurrence of
# this label, start to end" default the user can then freely narrow from by
# dragging the slider -- exactly this tab's "snap as a *starting point*, not
# a hard constraint" requirement. This mirrors getEventTimes()'s own
# first-occurrence/last-occurrence semantics (there, for two DIFFERENT event
# labels supplied by a caller who already knows which pair delimits a range
# of interest); here, with no such caller-supplied pairing available, using
# the SAME label's own first and last occurrence is the one derivation that
# needs no assumption about study-specific event-naming conventions (e.g.
# that a "TrialStart"/"TrialEnd" naming pattern exists at all) to still
# produce a sensible window for ANY event label.
#
# events: a data.frame with at least "event" and "recordingTimestamp_ms"
#   columns (as load_gaze_trajectory_data() already filters for before ever
#   returning a non-NULL `events`), or NULL/anything not shaped that way.
#
# Returns NULL if `events` isn't usable (missing, wrong shape, 0 rows after
# dropping NA event labels/timestamps), or a data.frame with columns event,
# n_occurrences, start_ms, end_ms -- one row per distinct event label,
# sorted by label.
derive_event_marker_windows <- function(events) {
  required_cols <- c("event", "recordingTimestamp_ms")
  if (is.null(events) || !is.data.frame(events) || !all(required_cols %in% names(events))) {
    return(NULL)
  }

  ev <- events[!is.na(events$event) & !is.na(events$recordingTimestamp_ms), required_cols, drop = FALSE]
  if (nrow(ev) == 0) {
    return(NULL)
  }

  ev %>%
    dplyr::group_by(event) %>%
    dplyr::summarise(
      n_occurrences = dplyr::n(),
      start_ms = min(recordingTimestamp_ms),
      end_ms = max(recordingTimestamp_ms),
      .groups = "drop"
    ) %>%
    dplyr::arrange(event) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

# points_in_polygon: point-in-polygon test via the standard even-odd
# ("ray casting") rule -- for every (x[i], y[i]), count how many edges of the
# polygon (poly_x/poly_y, in vertex order, implicitly closed from the last
# vertex back to the first) a horizontal ray cast rightward from that point
# crosses; an odd crossing count means the point is inside.
#
# A small, self-contained, pure-R implementation, rather than reaching for
# sf::st_within() (as the old, unintegrated app_lana.R reference script did)
# or sp::point.in.polygon(): sf in particular pulls in a real system-library
# dependency (GDAL/GEOS/PROJ) that's a genuine installation headache for an
# end user of an R *package* -- unlike app_lana.R's own single-analyst-
# machine, install-once context, where that cost was basically never felt. A
# rectangular/quadrilateral AOI test needs none of sf's actual geospatial
# machinery (map projections, geodesic distance, spatial indexing) -- it's a
# handful of lines of arithmetic -- so pulling in either heavier package here
# would trade a real install-time cost for no real capability this app
# needs. Not restricted to exactly 4 corners despite the AOI-definition UI
# (app.R) only ever offering 4 -- works for any simple (non-self-
# intersecting) polygon with >= 3 vertices, so a future richer AOI-drawing
# interaction isn't blocked by this function's own design.
#
# x, y: numeric vectors of equal length -- the points to test.
# poly_x, poly_y: numeric vectors of equal length (>= 3) -- the polygon's
#   vertices in order; the polygon is implicitly closed (an edge from the
#   last vertex back to the first is included even though poly_x/poly_y
#   don't repeat the first vertex at the end).
#
# Returns a logical vector, same length as x/y. NA in x or y at a given
# index yields NA (not FALSE) at that index -- callers that only want
# complete points considered are expected to subset x/y (and any companion
# columns) to non-NA pairs before calling this, since only they know which
# companion columns matter for that filtering (see compute_aoi_percent()
# below, which does exactly that).
points_in_polygon <- function(x, y, poly_x, poly_y) {
  n <- length(poly_x)
  if (n != length(poly_y) || n < 3) {
    stop("points_in_polygon: poly_x/poly_y must be equal-length vectors of at least 3 vertices")
  }
  if (length(x) != length(y)) {
    stop("points_in_polygon: x and y must be equal-length vectors")
  }

  na_point <- is.na(x) | is.na(y)
  inside <- rep(FALSE, length(x))

  # j starts as the "previous" vertex (the last one, since the polygon wraps
  # around) and walks forward alongside i; each (poly_x[j], poly_y[j]) ->
  # (poly_x[i], poly_y[i]) edge is tested for whether a rightward ray from
  # (x, y) crosses it, toggling `inside` (logical XOR, via `!inside[toggle]`)
  # once per crossing. Looping over polygon vertices (n, always small -- 4
  # for this tab's own AOI UI) rather than over points (x/y, potentially
  # thousands of gaze samples) keeps every per-edge operation below fully
  # vectorized across all points at once.
  j <- n
  for (i in seq_len(n)) {
    xi <- poly_x[i]
    yi <- poly_y[i]
    xj <- poly_x[j]
    yj <- poly_y[j]

    crosses <- (yi > y) != (yj > y)
    # suppressWarnings(): a horizontal edge (yi == yj) can produce a 0/0
    # NaN here, but `crosses` is deterministically FALSE for any such edge
    # (yi > y and yj > y can only differ when yi != yj), and R's `&`
    # short-circuits `FALSE & NA`/`FALSE & NaN`-comparisons to plain FALSE
    # -- so this NaN is always discarded, never propagated into `toggle`,
    # and the warning it would otherwise print is just noise.
    x_intersect <- suppressWarnings(xj + (xi - xj) * (y - yj) / (yi - yj))
    toggle <- crosses & (x < x_intersect)
    toggle[is.na(toggle)] <- FALSE
    inside[toggle] <- !inside[toggle]

    j <- i
  }

  inside[na_point] <- NA
  inside
}

# compute_aoi_percent: what fraction of a data.frame's non-missing gaze
# points fall inside a rectangular/quadrilateral AOI -- the Gaze Explorer
# tab's live AOI readout. Recomputed against whatever subset of rows the
# caller hands in (app.R hands in the current time-range-filtered trajectory
# data), so the percentage always reflects the SAME rows the trajectory plot
# itself is currently showing, not the whole recording.
#
# df: a data.frame with at least x_col/y_col numeric columns.
# aoi: NULL (no AOI defined yet -- returns NULL, not an error or a zero), or
#   a list(x = <numeric vector, >= 3>, y = <numeric vector, same length>) of
#   polygon vertices in the same units as x_col/y_col (see app.R's AOI
#   definition modal).
# x_col, y_col: column names in df to test (default the standard
#   preprocessed gaze columns every adapter's schema populates).
#
# Returns NULL if aoi is NULL, df is NULL/missing the required columns, or
# df has 0 usable (non-NA-coordinate) rows; otherwise a list: n_total (usable
# rows considered), n_inside, pct (0-1 fraction, n_inside / n_total).
compute_aoi_percent <- function(df, aoi, x_col = "gazeX.preprocessed_px", y_col = "gazeY.preprocessed_px") {
  if (is.null(aoi) || is.null(df) || !all(c(x_col, y_col) %in% names(df))) {
    return(NULL)
  }
  x <- df[[x_col]]
  y <- df[[y_col]]
  usable <- !is.na(x) & !is.na(y)
  x <- x[usable]
  y <- y[usable]
  if (length(x) == 0) {
    return(NULL)
  }

  inside <- points_in_polygon(x, y, aoi$x, aoi$y)
  n_inside <- sum(inside, na.rm = TRUE)
  list(n_total = length(x), n_inside = n_inside, pct = n_inside / length(x))
}

# gaze_trajectory_display_cap: the maximum number of gaze-sample points this
# tab ever hands to plotly for the trajectory trace itself -- a real
# high-frequency recording's selected time range can span tens of thousands
# of samples, and handing plotly's client-side JS that many points/DOM
# elements at once is what actually makes an interactive plot like this one
# visibly stutter or hang in a browser tab (unlike the static, server-
# rendered ggplot outputs in the "Plots" tab, which don't have this problem
# the same way). This caps the DISPLAYED trace only -- compute_aoi_percent()
# above always runs against the full, untrimmed filtered data.frame, never
# this thinned-for-display copy, so the reported AOI percentage is never
# silently wrong because of a rendering-only optimization.
gaze_trajectory_display_cap <- 5000L

# thin_for_display: evenly subsample df down to at most `cap` rows by row
# position -- purely a rendering-performance measure, see
# gaze_trajectory_display_cap's own comment. Assumes df is already sorted by
# time (build_gaze_trajectory_plot() below sorts before calling this), so an
# evenly-spaced positional subsample is also evenly spaced in time, not a
# biased sample from one portion of the range. Returns df unchanged when
# nrow(df) is already at or under the cap.
thin_for_display <- function(df, cap = gaze_trajectory_display_cap) {
  n <- nrow(df)
  if (n <= cap) {
    return(df)
  }
  idx <- unique(round(seq(1, n, length.out = cap)))
  df[idx, , drop = FALSE]
}

# build_gaze_trajectory_plot: an interactive plotly scatter/line of gaze
# position over time, with the current AOI (if any) overlaid as a filled
# polygon trace -- the Gaze Explorer tab's main view.
#
# A plotly, not ggplot2, output: unlike the ggplot-based "Plots" tab, this
# view needs live pan/zoom/hover on a scatter that's reactively rebuilt on
# every time-range slider move, and app_lana.R (the old, unintegrated
# reference script this tab's design is closest to) already established
# plotly as the right tool for exactly that interaction (this task's own
# scope decision).
#
# Colored (not just ordered) by time, via a continuous colorscale on the
# marker trace, plus connecting line segments in time order -- an animated,
# plotly-native frame-by-frame slider was considered and rejected as
# over-engineering here: the sliderInput this tab already has IS the
# time-range control, and stacking a second, plotly-native animation slider
# on top of it inside the same view would be a confusing double control for
# a modest gain over a static-but-interactive color gradient, which already
# makes directionality legible (color progression plus connecting lines)
# without needing to click/drag through frames.
#
# df: the (already time-range-filtered) trajectory data.frame -- see app.R's
#   filtered_trajectory() reactive. Must have time_col/x_col/y_col.
# aoi: NULL, or list(x = ..., y = ...) as compute_aoi_percent() expects --
#   overlaid as a semi-transparent filled polygon trace when present.
#
# Returns a plotly object, or NULL if df is NULL/missing the required
# columns, or has 0 usable (non-NA coordinate) rows -- callers should
# validate()/req() around that rather than handing plotly an empty trace.
build_gaze_trajectory_plot <- function(
    df, aoi = NULL,
    time_col = "recordingTimestamp_ms",
    x_col = "gazeX.preprocessed_px",
    y_col = "gazeY.preprocessed_px") {
  if (is.null(df) || !all(c(time_col, x_col, y_col) %in% names(df))) {
    return(NULL)
  }
  usable <- !is.na(df[[x_col]]) & !is.na(df[[y_col]]) & !is.na(df[[time_col]])
  df <- df[usable, , drop = FALSE]
  if (nrow(df) == 0) {
    return(NULL)
  }
  df <- df[order(df[[time_col]]), , drop = FALSE]
  plot_df <- thin_for_display(df)

  p <- plotly::plot_ly()
  p <- plotly::add_trace(
    p,
    x = plot_df[[x_col]], y = plot_df[[y_col]],
    type = "scatter", mode = "lines+markers",
    line = list(color = "rgba(120,120,120,0.35)", width = 1),
    marker = list(
      size = 6,
      color = plot_df[[time_col]],
      colorscale = "Viridis",
      showscale = TRUE,
      colorbar = list(title = "Time (ms)")
    ),
    text = paste0("t = ", round(plot_df[[time_col]]), " ms"),
    hoverinfo = "text+x+y",
    name = "Gaze path"
  )

  # AOI overlay: a second trace tracing the polygon's corners back to its
  # own first corner (closing it), filled ("toself") so the region itself is
  # visually obvious, not just its outline -- works the same way for any
  # simple polygon this tab hands in, not just a rectangle, matching
  # points_in_polygon()'s own not-rectangle-specific design.
  if (!is.null(aoi) && length(aoi$x) >= 3 && length(aoi$x) == length(aoi$y)) {
    p <- plotly::add_trace(
      p,
      x = c(aoi$x, aoi$x[1]), y = c(aoi$y, aoi$y[1]),
      type = "scatter", mode = "lines",
      fill = "toself", fillcolor = "rgba(220,20,60,0.15)",
      line = list(color = "crimson", width = 2),
      name = "AOI", hoverinfo = "skip"
    )
  }

  plotly::layout(
    p,
    xaxis = list(title = paste(x_col)),
    yaxis = list(title = paste(y_col)),
    showlegend = TRUE
  )
}

# ---------------------------------------------------------------------------
# Shared per-tab file selector: a search/dropdown control repeated at the top
# of the QC flags, Plots, and Gaze Explorer tabs (three separate widget
# instances -- a single Shiny input can't render itself in three places on
# one page at once -- but all three are kept in sync in app.R against the
# same underlying selected_source_file_val() reactiveVal that P10-03's QC
# table row-click already populates, so picking a file in any one instance,
# or clicking a QC table row, is reflected in the others).
# ---------------------------------------------------------------------------

# build_file_selector_choices: named-vector choices for that selector --
# names are what a user reads (the recording id, or "recording [batch_name]"
# when the same recording id is genuinely ambiguous across >1 batch_name),
# values are that row's source_file path (the same identity
# selected_source_file_val already tracks).
#
# The ambiguous-recording disambiguation rule mirrors
# build_qc_comparison_plot()'s own bar_label construction exactly (append
# "[batch_name]" only when needed), but is written out again here rather
# than factored into one shared helper both call -- build_qc_comparison_plot()
# is already tested/working, and this task doesn't need to touch its
# internals to add a new, independent consumer of the same small rule.
#
# table: the combined qcsummary table (current_load_result()$table), or
#   NULL/0-row/missing the required identity columns.
#
# Returns a named character vector (names = display labels, values =
# source_file paths), sorted by label, or character(0).
build_file_selector_choices <- function(table) {
  required_cols <- c("recording", "batch_name", "source_file")
  if (is.null(table) || nrow(table) == 0 || !all(required_cols %in% names(table))) {
    return(character(0))
  }

  files <- unique(table[, required_cols, drop = FALSE])
  ambiguous <- names(which(tapply(files$batch_name, files$recording, function(b) length(unique(b))) > 1))
  files$label <- ifelse(
    files$recording %in% ambiguous,
    paste0(files$recording, " [", ifelse(is.na(files$batch_name), "NA", files$batch_name), "]"),
    files$recording
  )
  files <- files[order(files$label), , drop = FALSE]
  stats::setNames(files$source_file, files$label)
}

# ---------------------------------------------------------------------------
# "Major" QC metrics: the same 3-concept/4-row set qc_threshold_config
# already treats as the unambiguous, universal pass/fail signals (P10-02) --
# reused here (not redefined) as the compact "glance at this file" summary
# shown next to the Plots/Gaze Explorer file selectors, and as the columns
# of the QC flags tab's own compact per-file table.
# ---------------------------------------------------------------------------

# major_qc_metric_display_labels: short, per-metric-ROW display labels for
# that compact summary -- deliberately its own small map rather than reusing
# qc_threshold_config$label directly, since that column's labels describe
# the shared THRESHOLD CONTROL (interpolated_LeftEye and interpolated_RightEye
# intentionally share one label there, "Maximum % interpolated data (either
# eye)", because they share one UI numericInput -- see that config's own
# comment) where here each of the 4 qc_metric rows needs its OWN distinct,
# short label so a two-badge "Interpolated (L)" / "Interpolated (R)" readout
# doesn't show the same text twice.
major_qc_metric_display_labels <- c(
  valid_raw_data = "Valid raw data",
  robustness_proportion_valid_data_to_all_data = "Robust data",
  interpolated_LeftEye = "Interpolated (L)",
  interpolated_RightEye = "Interpolated (R)"
)

# major_qc_metrics: the qc_metric values major_qc_metric_display_labels
# covers -- a thin accessor so callers never need to know that map's
# internal shape (names() vs. values()) to get the plain metric-name vector
# build_qc_comparison_table()/table-filtering callers actually want.
major_qc_metrics <- function() {
  names(major_qc_metric_display_labels)
}

# build_major_metrics_summary: the major metrics for exactly ONE file, as a
# small label/value/flag data.frame -- backs the compact "is this file okay
# at a glance" readout shown beside the Plots/Gaze Explorer file selectors.
#
# table_flagged: the combined qcsummary table with a qc_flag column already
#   attached (app.R's qc_table_flagged() reactive).
# source_file: the single file to summarize, or NULL/"" (returns NULL, the
#   "nothing selected yet" case every caller already needs to handle).
#
# Returns a data.frame(label, percent, qc_flag) -- one row per major metric
# with a matching row in table_flagged for that file -- or NULL if
# source_file is missing, or has none.
build_major_metrics_summary <- function(table_flagged, source_file) {
  if (is.null(table_flagged) || is.null(source_file) || !nzchar(source_file)) {
    return(NULL)
  }
  rows <- table_flagged[
    table_flagged$source_file == source_file & table_flagged$qc_metric %in% major_qc_metrics(),
    ,
    drop = FALSE
  ]
  if (nrow(rows) == 0) {
    return(NULL)
  }
  data.frame(
    label = unname(major_qc_metric_display_labels[rows$qc_metric]),
    percent = rows$percent,
    qc_flag = rows$qc_flag,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Per-file review notes: one shared notes.tsv per loaded output directory.
#
# A reviewer's free-text note (e.g. "excessive blinking during calibration,
# re-run recommended") attached to a specific recording, persisted so it
# survives an app restart or a later reload of the same directory, and
# included in the "export flagged for review" CSV (build_flagged_export_table()
# above) alongside whichever metrics actually triggered the flag.
#
# ONE file per loaded directory (not one sidecar per recording, and not
# folded into qcsummary.tsv/batch_config.yaml): simpler to find/back up/
# version than scattering a per-recording file, and directly matches this
# feature's stated usage pattern -- multiple reviewers working on the same
# study's output directory over time, just never at the exact same moment
# -- so there's no locking or conflict-merge logic here at all: "Save note"
# always reads the CURRENT in-memory notes table, updates it, and
# immediately overwrites the whole file on disk, which is the correct,
# simplest behavior as long as two reviewers are never doing that in the
# same instant.
# ---------------------------------------------------------------------------

# notes_filename: eyeQuality-prefixed (not a bare "notes.tsv") specifically
# to avoid silently colliding with an unrelated file a user might already
# have sitting at the root of their own output directory.
notes_filename <- "eyeQuality_analyze_notes.tsv"

# resolve_notes_path: the shared notes file's path for a given loaded output
# directory -- always at that directory's own root, regardless of how many
# levels down discover_qcsummary_files() actually found qcsummary.tsv
# outputs under it (P10-01's recursive search) -- one notes file per
# directory the app was pointed at, not one per subdirectory it happened to
# search into.
resolve_notes_path <- function(directory) {
  file.path(directory, notes_filename)
}

# empty_notes_table: the canonical 0-row shape every notes.tsv read/write
# path returns or expects. recording/batch_name identify a file the same way
# every other per-recording view in this app already does (P10-01's
# read_one_qcsummary()); note is the free text; updated_at is an ISO-8601-ish
# timestamp of the most recent edit -- informational only (nothing here
# reads it back programmatically), but a reviewer opening notes.tsv directly
# wants to know how stale a note is.
empty_notes_table <- function() {
  data.frame(
    recording = character(0), batch_name = character(0),
    note = character(0), updated_at = character(0),
    stringsAsFactors = FALSE
  )
}

# load_notes_table: read a directory's shared notes.tsv, or fall back to
# empty_notes_table() if it doesn't exist yet (the common case: no reviewer
# has left a note in this directory yet) or fails to parse/is missing an
# expected column (a hand-edited or otherwise corrupted file) -- degrades
# rather than blocking the whole app on a notes file, which is secondary to
# the actual QC data this app exists to review.
load_notes_table <- function(directory) {
  path <- resolve_notes_path(directory)
  if (!windows_safe_file_exists(path)) {
    return(empty_notes_table())
  }
  tryCatch(
    {
      tbl <- windows_safe_read_tsv(path, show_col_types = FALSE, progress = FALSE)
      if (!all(c("recording", "batch_name", "note") %in% names(tbl))) {
        return(empty_notes_table())
      }
      tbl
    },
    error = function(e) empty_notes_table()
  )
}

# save_notes_table: overwrite a directory's notes.tsv with the given table.
# Called immediately on every "Save note" click (app.R), not batched or
# debounced -- notes.tsv is small (one row per annotated recording, not per
# sample), so there's no performance reason to defer the write, and
# immediate persistence is what actually makes "multiple reviewers, never
# simultaneous" work in practice: the next reviewer to open this directory
# sees the previous one's notes without any separate export/sync step.
save_notes_table <- function(notes_df, directory) {
  windows_safe_write_tsv(notes_df, resolve_notes_path(directory))
}

# upsert_note: update, insert, or remove a single (recording, batch_name)
# row's note. A blank/whitespace-only `note` REMOVES any existing row for
# that file instead of writing an empty note -- keeps notes.tsv from
# accumulating empty rows for every file a reviewer merely glanced at (only
# files someone actually wrote something about are ever persisted), and
# lets clearing the textbox actually clear the saved note rather than
# leaving a blank-but-present row behind.
#
# batch_name may legitimately be NA (derive_batch_name()'s documented "no
# batchName" naming form) -- matched via an explicit is.na() branch rather
# than plain `==`, which would silently produce NA (never TRUE) against a
# real NA batch_name and so never find/replace that row.
#
# notes_df: a table shaped like empty_notes_table()'s return value (NULL is
#   treated the same as empty_notes_table()).
# recording, batch_name: identify which row to update/insert/remove.
# note: the new note text.
#
# Returns the updated notes table.
upsert_note <- function(notes_df, recording, batch_name, note) {
  if (is.null(notes_df)) {
    notes_df <- empty_notes_table()
  }
  same_recording <- notes_df$recording == recording &
    (if (is.na(batch_name)) is.na(notes_df$batch_name) else notes_df$batch_name %in% batch_name)
  notes_df <- notes_df[!same_recording, , drop = FALSE]

  if (is.null(note) || !nzchar(trimws(note))) {
    return(notes_df)
  }

  rbind(
    notes_df,
    data.frame(
      recording = recording, batch_name = batch_name, note = note,
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      stringsAsFactors = FALSE
    )
  )
}

# note_for_recording: the current note text for a (recording, batch_name)
# pair, or "" if none exists -- used to populate the notes textAreaInput
# whenever the selected file changes, so switching files shows THAT file's
# own previously-saved note (if any) rather than appearing to carry over
# whatever was typed for the last file selected.
note_for_recording <- function(notes_df, recording, batch_name) {
  if (is.null(notes_df) || is.null(recording) || nrow(notes_df) == 0) {
    return("")
  }
  same_recording <- notes_df$recording == recording &
    (if (is.na(batch_name)) is.na(notes_df$batch_name) else notes_df$batch_name %in% batch_name)
  match <- notes_df[same_recording, , drop = FALSE]
  if (nrow(match) == 0) "" else match$note[1]
}

# ---------------------------------------------------------------------------
# Compare files tab: 2-metric scatterplot
# ---------------------------------------------------------------------------
#
# build_qc_comparison_plot() (P10-04, above) answers "how does ONE metric
# stack up across every file" as a bar chart. Selecting a SECOND metric in
# the same control switches the view to a scatterplot instead -- one point
# per file, one metric per axis -- which is what actually answers "do files
# that are bad on metric A also tend to be bad on metric B" at a glance,
# something no number of single-metric bar charts can show directly.

# build_qc_comparison_scatter: metric_x vs. metric_y, one point per file,
# colored by whether that file is flagged on EITHER of the two metrics being
# plotted (table_flagged's own qc_flag column -- never recomputed here,
# same "single source of truth" discipline build_qc_comparison_plot()
# already follows).
#
# table_flagged: the combined qcsummary table with a qc_flag column already
#   attached (app.R's qc_table_flagged() reactive, already narrowed to
#   whichever batch_name(s) are in view via filter_by_batch_name() before
#   this is called -- same convention as build_qc_comparison_plot()).
# metric_x, metric_y: two distinct qc_metric values (one per axis).
# thresholds: current threshold values (app.R's qc_thresholds() reactive) --
#   used only to draw a dashed reference line on whichever axis/axes have a
#   configured threshold, exactly like build_qc_comparison_plot()'s own
#   geom_hline().
#
# Returns a ggplot object, or NULL if metric_x/metric_y are missing,
# identical, or build_qc_comparison_table() (reused here for the actual
# long-to-wide pivot, rather than a second pivot implementation) has no
# usable rows for them.
build_qc_comparison_scatter <- function(table_flagged, metric_x, metric_y, thresholds) {
  if (is.null(table_flagged) || is.null(metric_x) || is.null(metric_y) ||
    !nzchar(metric_x) || !nzchar(metric_y) || identical(metric_x, metric_y)) {
    return(NULL)
  }

  wide <- build_qc_comparison_table(table_flagged, c(metric_x, metric_y))
  if (is.null(wide) || !all(c(metric_x, metric_y) %in% names(wide))) {
    return(NULL)
  }

  flag_lookup <- table_flagged[table_flagged$qc_metric %in% c(metric_x, metric_y), c("source_file", "qc_flag")]
  flagged_files <- unique(flag_lookup$source_file[flag_lookup$qc_flag])
  wide$comparison_status <- factor(
    ifelse(wide$source_file %in% flagged_files, "Flagged", "OK"),
    levels = c("Flagged", "OK")
  )

  plot <- ggplot2::ggplot(
    wide,
    ggplot2::aes(x = .data[[metric_x]], y = .data[[metric_y]], color = comparison_status)
  ) +
    ggplot2::geom_point(size = 3, alpha = 0.85) +
    ggplot2::scale_color_manual(values = c(Flagged = "#c0392b", OK = "#27ae60"), drop = FALSE) +
    # Both axes are 0-1 fractions (calculateOutputMetrics()'s "percent"
    # column), labeled as percentages here purely for display, matching
    # build_qc_comparison_plot()'s own axis convention.
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::labs(
      x = metric_x, y = metric_y, color = "QC status",
      title = paste("Across-file comparison:", metric_x, "vs.", metric_y)
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 13),
      axis.title = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      legend.text = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14)
    )

  cfg_x <- qc_threshold_config[qc_threshold_config$qc_metric == metric_x, , drop = FALSE]
  if (nrow(cfg_x) == 1) {
    tv <- thresholds[[cfg_x$threshold_id[1]]]
    if (!is.null(tv) && !is.na(tv)) {
      plot <- plot + ggplot2::geom_vline(xintercept = tv, linetype = "dashed", color = "black")
    }
  }
  cfg_y <- qc_threshold_config[qc_threshold_config$qc_metric == metric_y, , drop = FALSE]
  if (nrow(cfg_y) == 1) {
    tv <- thresholds[[cfg_y$threshold_id[1]]]
    if (!is.null(tv) && !is.na(tv)) {
      plot <- plot + ggplot2::geom_hline(yintercept = tv, linetype = "dashed", color = "black")
    }
  }

  plot
}
