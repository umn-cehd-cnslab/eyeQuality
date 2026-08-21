#' Prefix a path for Windows' extended-length path API
#'
#' Windows' classic file APIs enforce a ~260 character `MAX_PATH` limit on a
#' fully-resolved path. `file.exists()`/`list.files()` go through a
#' *different* Windows API that has no such limit, which is exactly why a
#' file over this limit can be found/listed as present and then still fail
#' to open with a plain "does not exist" error -- the file is real, only
#' the classic open call is refusing it. This is easy to hit in practice on
#' deeply-nested Box/OneDrive-synced study trees, compounded by this
#' package's own nested `derivatives/eyeQuality-v1/` output convention and
#' long BIDS-style filenames.
#'
#' Windows' documented workaround is the `\\?\` extended-length path prefix,
#' which opts a path into the Win32 API family that has no `MAX_PATH`
#' limit. It comes with real constraints this function has to satisfy:
#' - it requires an **absolute, backslash-separated** path -- the `\\?\`
#'   form disables normal path parsing (no `/`, no `.`/`..` resolution), so
#'   the path must already be fully resolved before the prefix is applied;
#' - a **UNC** path (`\\server\share\...`) needs `\\?\UNC\server\share\...`,
#'   not a bare `\\?\` glued directly onto the `\\` form;
#' - it means nothing (or something actively wrong) off Windows, so it must
#'   be a no-op there;
#' - applying it to a path that's already in this form would double-prefix
#'   it into something invalid.
#'
#' `normalizePath(mustWork = FALSE)` is the obvious way to get an absolute,
#' resolved path before adding the prefix. An earlier version of this
#' function trusted it unconditionally; a sibling fix in this codebase
#' (`inst/shiny-apps/analyze/helpers.R`'s own `windows_long_path()`) reported
#' `normalizePath(mustWork = FALSE)` silently failing to resolve a long,
#' not-yet-existing path on its own testing. Independent re-verification here
#' (multiple real >250-character paths, both single giant segments and real
#' multi-segment nested trees) could **not** reproduce that specific failure
#' -- `normalizePath()` resolved correctly every time it was tried on this
#' package's own Windows R test environment. Rather than delete the fallback
#' on the strength of a non-reproduction (the underlying claim may still be
#' real under conditions not yet tried, e.g. a different Windows/R build), it
#' stays as cheap defense in depth: a manual fallback
#' (`.windows_force_backslash()`, `.windows_is_absolute_path()`,
#' `.windows_collapse_dot_segments()` below) runs after `normalizePath()`
#' unconditionally, confirmed harmless (a no-op) on the success path this
#' function's own tests exercise, and would still catch the reported failure
#' mode if it does occur.
#'
#' Note this function alone is **not** sufficient to make `readr` (or
#' `vroom`, which `readr` is built on) actually read a `\\?\`-prefixed long
#' path -- see `windows_safe_read_csv()`/`windows_safe_read_tsv()`/
#' `windows_safe_read_lines()` below, and their shared header comment, for
#' why and for the real fix used at every long-path-vulnerable call site in
#' this package.
#'
#' @param path a file path; may be relative, absolute, or already in
#'   extended-length form.
#' @param os_type internal/testing hook for which platform `path` should be
#'   treated as belonging to. Defaults to `.Platform$OS.type` (the actual
#'   running platform); callers should not normally supply this -- it exists
#'   so this function's Windows-only branches can be exercised by tests on a
#'   non-Windows machine.
#'
#' @return `path` unchanged when `os_type` is not `"windows"`, or when
#'   `path` is already `\\?\`- (or `\\?\UNC\`-) prefixed. Otherwise `path`
#'   resolved to an absolute, backslash-separated form and prefixed with
#'   `\\?\` (`\\?\UNC\` if `path` is a UNC path).
#' @keywords internal
#' @noRd
windows_long_path <- function(path, os_type = .Platform$OS.type) {
  if (!identical(os_type, "windows")) {
    return(path)
  }
  if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(path)
  }

  extended_prefix <- "\\\\?\\" # literal \\?\

  if (startsWith(path, extended_prefix)) {
    # already extended-length (or extended-length UNC) form -- applying the
    # prefix again would produce an invalid, double-prefixed path
    return(path)
  }

  # Detect UNC-ness off the *original* string, before any resolution:
  # normalizePath()'s underlying OS path-resolution call collapses a
  # doubled leading separator once the path actually exists (verified: a
  # doubled leading "//" on an existing path comes back as a single "/"),
  # which would silently erase the one signal that distinguishes "share on
  # a network host" from an ordinary absolute path if read off the
  # resolved result instead.
  forward_slash_path <- gsub("\\\\", "/", path)
  is_unc <- grepl("^//", forward_slash_path)

  # normalizePath(mustWork = FALSE) resolves relative paths and "."/".."
  # segments to an absolute form without erroring if the path can't be
  # stat()'d -- important here because the whole point of this function is
  # to help *open* paths whose ordinary MAX_PATH-limited resolution may
  # already be straining against the same 260-character limit; a hard
  # mustWork = TRUE failure would just move the bug rather than fix it.
  resolved <- tryCatch(
    normalizePath(forward_slash_path, winslash = "/", mustWork = FALSE),
    error = function(e) forward_slash_path
  )

  # Manual second pass -- see this function's header comment on why this
  # can't be skipped just because normalizePath() already ran: for a path
  # that doesn't yet exist on disk, normalizePath(mustWork = FALSE) can
  # silently leave it relative and dot-segment-laden.
  resolved <- .windows_force_backslash(resolved)
  if (!.windows_is_absolute_path(resolved)) {
    cwd <- sub("\\\\+$", "", .windows_force_backslash(getwd()))
    resolved <- paste0(cwd, "\\", resolved)
  }
  resolved <- .windows_collapse_dot_segments(resolved)

  if (startsWith(resolved, extended_prefix)) {
    return(resolved)
  }

  if (is_unc || startsWith(resolved, "\\\\")) {
    # \\?\UNC\ takes a single backslash before "server\share\...", not the
    # doubled one an ordinary "\\server\share" form uses -- strip whatever
    # is there (one or two leading backslashes, depending on whether the
    # path existed at resolution time) and rebuild it explicitly.
    paste0(extended_prefix, "UNC\\", sub("^\\\\+", "", resolved))
  } else {
    paste0(extended_prefix, resolved)
  }
}

# .windows_force_backslash: replace every "/" with a single "\" -- R's own
# path helpers (file.path(), and by extension list.files(full.names =
# TRUE), which is how nearly every path windows_long_path() ever sees
# actually arrives) default to "/" as their separator even on Windows, and
# normalizePath()'s winslash = "\\" argument can silently no-op on a long,
# not-yet-existing path -- so this has to be redone by hand rather than
# assumed already done. The replacement string below ("\\", two characters
# in the R source) is a single backslash character -- everything
# downstream (.windows_is_absolute_path()/.windows_collapse_dot_segments())
# assumes single-backslash-separated input, so this must not double it.
.windows_force_backslash <- function(p) {
  gsub("/", "\\", p, fixed = TRUE)
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
# so any such segments have to be gone before windows_long_path() adds that
# prefix. Also compensates for the case (see windows_long_path()'s own
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

#' Safe long-path readr wrappers (`read_csv`/`read_tsv`/`read_lines`)
#'
#' A drop-in replacement for calling `readr::read_csv()`/`readr::read_tsv()`/
#' `readr::read_lines()` directly at any call site in this package that
#' reads a file by path.
#'
#' `windows_long_path()` on its own is **not enough**: confirmed directly
#' against a real long-path fixture on native Windows R, `readr::read_csv()`/
#' `read_tsv()`/`read_lines()` do NOT reliably honor a `\\?\`-prefixed
#' extended-length path passed to them as a plain string. `readr` (edition 2,
#' the current default) is backed by `vroom`, which in turn leans on the `fs`
#' package for its own path handling/existence checks; both `readr`'s and
#' `vroom`'s own `standardise_path()` pre-open existence check
#' (`file.exists(path)`) returns `FALSE` for a genuinely-existing
#' `\\?\`-prefixed path, so the read fails with "does not exist" before the
#' actual C++ reading code is ever reached -- the file-open call itself was
#' never the problem here, the pre-open R-level existence check is.
#'
#' What DOES work, confirmed directly on the same real long-path fixture:
#' base R's own `file()` connections correctly honor a `\\?\`-prefixed path
#' (`readLines()`/`read.delim()` succeed on it directly), and `readr`'s
#' `read_csv()`/`read_tsv()`/`read_lines()` all accept an already-open
#' connection in place of a path string (`standardise_path()`/`vroom`'s
#' `standardise_path()` both special-case `is.connection(file)` before any
#' existence check runs). So the fix is to have BASE R (not readr/vroom/fs)
#' do the actual file opening -- these wrappers open a binary connection
#' themselves via `windows_long_path()`, and hand `readr` that connection
#' rather than the path string, sidestepping `fs`'s mangling entirely since
#' its path logic is never consulted for a connection object.
#'
#' A plain, unprefixed `readr::read_csv()`/`read_tsv()`/`read_lines()` call
#' is used on non-Windows platforms (`windows_long_path()` is a no-op there,
#' so opening a redundant connection ourselves would add no value, just
#' another thing that could behave slightly differently from calling readr
#' directly).
#'
#' The connection is explicitly closed on exit (`readr`'s read functions do
#' NOT close a connection they did not open themselves) via `on.exit()`
#' rather than a plain trailing `close()`, so it's still released on an
#' error partway through the read (avoiding a leaked, potentially locking,
#' open file handle).
#'
#' Opening the connection itself is wrapped so that a failure to open (e.g.
#' a missing file) raises a clear, specific error -- `file()`'s own default
#' failure behavior is to first emit a warning with the actual OS reason
#' (e.g. "No such file or directory") and only then raise a generic "cannot
#' open the connection" error whose message alone loses that detail; the
#' warning handler intercepts at the point the specific reason is still
#' available and re-raises it as the error instead.
#'
#' @param path a single file path (any length).
#' @param ... forwarded to the underlying `readr` function (e.g.
#'   `show_col_types`, `guess_max`).
#'
#' @return Whatever the underlying `readr` function returns (or raises
#'   whatever error it, or the connection-open step, raises).
#' @name windows_safe_read
#' @keywords internal
#' @importFrom readr read_csv
#' @importFrom readr read_tsv
#' @importFrom readr read_lines
#' @noRd
NULL

#' @rdname windows_safe_read
#' @noRd
windows_safe_read_csv <- function(path, ...) {
  if (.Platform$OS.type != "windows") {
    return(read_csv(path, ...))
  }
  con <- tryCatch(
    file(windows_long_path(path), open = "rb"),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  )
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  read_csv(con, ...)
}

#' @rdname windows_safe_read
#' @noRd
windows_safe_read_tsv <- function(path, ...) {
  if (.Platform$OS.type != "windows") {
    return(read_tsv(path, ...))
  }
  con <- tryCatch(
    file(windows_long_path(path), open = "rb"),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  )
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  read_tsv(con, ...)
}

#' @rdname windows_safe_read
#' @noRd
windows_safe_read_lines <- function(path, ...) {
  if (.Platform$OS.type != "windows") {
    return(read_lines(path, ...))
  }
  con <- tryCatch(
    file(windows_long_path(path), open = "rb"),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  )
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  read_lines(con, ...)
}

#' `basename()`-equivalent with no fixed-size internal path buffer
#'
#' A third, distinct long-path failure point, confirmed directly (not
#' assumed) on the project's authoritative Windows R environment, independent
#' of both the classic Win32 `MAX_PATH` story `windows_long_path()` works
#' around and the `readr`/`vroom`/`fs` mangling `windows_safe_read_csv()`/
#' `windows_safe_read_tsv()`/`windows_safe_read_lines()` work around: base R's
#' own `basename()` (and `dirname()`) raise a plain "path too long" error for
#' a path around 300 characters, on a string that `list.files()`/
#' `windows_safe_read_tsv()` etc. already handle correctly -- apparently R's
#' Windows build's own internal fixed-size path buffer for these two
#' functions specifically, not a restatement of either issue above. Left
#' unfixed, `create_new_filename()` (`R/saveFiles.R`) and `.processFilename()`
#' (`R/eyeQualityBatch.R`) would still fail on a real long input file path
#' even once every `readr`-based read/write in this package is fixed.
#'
#' Implemented as pure string manipulation with no underlying OS/file-API
#' call at all, so there's no length-limited buffer left for it to hit.
#'
#' @param path a file path (any length, any platform).
#'
#' @return The final path component (same semantics as `basename()`), or
#'   `NA_character_` for `NA` input.
#' @keywords internal
#' @noRd
.safe_basename <- function(path) {
  if (is.na(path)) {
    return(NA_character_)
  }
  path <- sub("[/\\\\]+$", "", path)
  m <- regmatches(path, regexpr("[^/\\\\]*$", path))
  if (length(m) == 0) "" else m
}
