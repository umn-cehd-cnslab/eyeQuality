# eyeQuality — Detailed Fix / Build Checklist

Based on a full read-through of `R/*.R` in elab-umn/eyeQuality (47 commits, main branch,
scanned 2026-07-29). Organized so you can check items off directly. Each bug item lists
the file so it's easy to jump to.

**One encouraging finding up front:** the pipeline core (`eyeSelection.R`,
`classifyBlinks.R`, `calculateFrequency_hz.R`, `checkGazeData.R`, `interpolateGaze.R`,
`classifyGazeIVT.R`, `calculateOutputMetrics.R`) already operates on *standardized*
column names and is device-agnostic. The Tobii-specific logic is concentrated in a
small number of "seam" files. That means the eye-tracker generalization (Section E)
is more tractable than it might sound — it's an isolation/refactor job, not a rewrite.

---

## A. Confirmed bugs / correctness issues

- [ ] **`eyeQualityBatch.R` (batch parallel call) — hardcoded display dimensions.**
  `parLapply()` calls `eyeQuality(x, displayDimensionX_mm = 594, displayDimensionY_mm = 344, ...)`
  with literal values, ignoring whatever the user configured. The README's own example
  (`batch_preprocess(..., display.dimx = 344, display.dimy = 344)`) would not work as documented —
  either silently uses wrong screen dims, or errors on duplicate arg if also passed via `...`.
- [ ] **`eyeQualityBatch.R` — `numberCores` / `batchName` not validated.** No check that
  `batchName` is non-empty/non-NULL (used to build filenames); no check `numberCores` is a
  positive integer.
- [ ] **`extractEventRows.R` — no `else` branch.** If `software` isn't exactly `"TobiiPro"` or
  `"TobiiStudio"`, `gazeStreamData`/`eventData` are never assigned and the function fails with
  an opaque "object not found" error instead of a clear message.
- [ ] **`parsePreprocessingBatchSummary.R` — `"failedfiles"` branch is an empty stub`** (only
  `"summary"` is implemented; `"successfulfiles"` isn't handled either, despite being named
  in the roxygen `@param` docs).
- [ ] **`saveFiles.R` / `create_new_filename()` — output location is hardcoded** to
  `<input_dir>/derivatives/eyeQuality-v1/`. No `outputDir` parameter exists anywhere in the
  call chain (`eyeQuality()` → `saveFiles()` → `create_new_filename()`). Users cannot write
  outputs to a different drive/location without post-hoc file moves.
- [ ] **`removeInvalidGaze.R`** has an explicit `#FIXME: this function is overwriting the
  columns. we should create new columns?` — worth resolving before calling the package "done."
- [ ] **`listBidsFiles.R`** silently drops any directory that doesn't match
  `subjectPattern_regex`/`sessionPattern_regex` (via `next`), only logging via `print()`. In a
  batch context this is easy to miss — consider surfacing a structured "skipped N directories"
  count in the batch summary.
- [ ] **`eyeQuality.R` argument-logging block** (`argList <- paste0("eyeQuality(", ...)`) will
  error or produce a garbled string if `args` contains a data.frame (e.g., when `data` is
  passed directly instead of `filepath`) — `stringr::str_glue("{names(args)} = {args}")` isn't
  safe for non-atomic values.
- [ ] **CRLF line endings** in `R/*.R` (confirmed via grep) — mixed with the `.gitattributes`
  file; worth confirming `.gitattributes` normalizes this consistently across contributor OSes,
  since it can cause noisy diffs and roxygen inconsistencies.
- [ ] **Dead/commented-out code** — large commented-out blocks of alternate `dplyr::rename()`
  syntax in `renameColumns.R` (~80 lines of comments for ~20 lines of active code). Clean up
  before CRAN submission (R CMD check will not fail on this, but it's a maintainability and
  professionalism issue for a package you're pointing external users to).
- [ ] **`eyeQuality()`'s own internal timing/duration blocks are extremely repetitive** (~140
  lines of near-identical `calculateTimeDifference()` calls) — not a bug, but a good candidate
  for a small internal helper (`buildTimingList(runtime_vector)`) to reduce copy-paste-error risk
  when new pipeline steps are added.
- [ ] Audit every other `str_detect(software, ...)` / `software ==` branch point for the same
  "no else / no fallback" issue as `extractEventRows.R`: confirmed present in
  `detectImportSourceType.R` (has `stop()`, OK), `standardizeColumnNames.R` (has `stop()`, OK),
  `removeInvalidGaze.R` (**no else/stop — silently returns data unmodified if software doesn't
  match**, confirm and fix).

---

## B. CRAN readiness checklist

- [ ] `DESCRIPTION`: replace placeholder `Title: What the Package Does (One Line, Title Case)`
  with a real, CRAN-style title (Title Case, no period, ideally ≤65 chars).
- [ ] `DESCRIPTION`: expand `Description:` field to a full paragraph (CRAN wants >1 sentence,
  explaining what the package does without restating the title).
- [ ] `DESCRIPTION`: confirm all `Imports:` packages are actually used (spot check —
  `forecast`, `pracma`, `data.table` are used in `classifyBlinks.R`; verify the rest) and that
  none used in code are missing from `Imports`.
- [ ] Add `Suggests: knitr, rmarkdown` (needed once vignettes exist) and `VignetteBuilder: knitr`.
- [ ] Bump `Version` to `0.1.0` (or similar) — `1.0.0.0` is a 4-part version, which is
  non-standard for CRAN (expects `x.y.z` or `x.y.z.w` is technically allowed but unusual for
  a first release; consider `0.1.0` to signal "first CRAN release").
- [ ] Add `NEWS.md` documenting at least the changes since inception, or at minimum a
  "0.1.0 Initial CRAN release" entry.
- [ ] Add `cran-comments.md` (template: R CMD check results, test environments, downstream
  dependencies note — "This is a new release.").
- [ ] Run `devtools::check()` locally and resolve every ERROR/WARNING/NOTE.
- [ ] Run `devtools::check_win_devel()` and `rhub::rhub_check()` (or `R-hub` v2) across
  multiple platforms (Windows, macOS, Linux) — package uses `parallel::makeCluster(type="FORK")`
  which is Unix-only, guarded correctly by `.Platform$OS.type`, but confirm this code path is
  actually exercised in CI on both platforms.
- [ ] Confirm no functions write to the user's filesystem without permission during
  `R CMD check` / examples / tests (CRAN policy) — `saveFiles()`/`create_new_filename()` create
  directories; make sure examples/tests use `tempdir()`, not the working directory.
- [ ] Check all `@examples` in roxygen docs are runnable (`devtools::run_examples()`) —
  currently only `calculateFrequency_hz.R` has an example block; CRAN doesn't strictly require
  examples on every function but reviewers appreciate them on the two main entry points
  (`eyeQuality()`, `eyeQualityBatch()`).
- [ ] Spell-check DESCRIPTION and Rd files (`devtools::spell_check()`).
- [ ] Confirm `LICENSE.md` (GPL-3 full text) vs. required CRAN `LICENSE` file format — CRAN
  wants a short `LICENSE` file for `GPL (>=3)` referencing the license, not necessarily the
  full text at top level (`.Rbuildignore` already excludes `LICENSE.md`, confirm this is
  intentional and a proper CRAN-format license file exists).
- [ ] Verify package builds with `--as-cran` and passes `R CMD check` with **zero** notes
  about "no visible binding for global variable" (likely present given heavy `.data$` /
  bare-name mixing seen in `renameColumns.R` and elsewhere — audit consistent use of
  `.data$` or `rlang::.data` pronoun throughout, especially in the commented-vs-active
  code in `renameColumns.R` where some blocks use `.data$X` and the active block doesn't).
- [ ] Confirm `LazyData: true` is actually needed (no `data/` directory currently exists in
  repo — if there's no exported package data, this line should be removed).

---

## C. Testing checklist

- [ ] Create `tests/testthat/` (currently does not exist despite `Config/testthat/edition: 3`
  and README pointing to a nonexistent `tests/README.md`).
- [ ] Build synthetic fixture datasets (small, checked into `tests/testthat/fixtures/` or
  generated on the fly) for:
  - [ ] Minimal valid Tobii Studio export (few hundred rows, both eyes present)
  - [ ] Minimal valid Tobii Pro export
  - [ ] Monocular data (one eye entirely missing) for both formats
  - [ ] A run containing a clean blink (short gap) vs. a long dropout (not a blink)
  - [ ] Out-of-order timestamps (to exercise `checkOrderedTimestamps` abort path)
  - [ ] Empty / all-NA file (to exercise `checkGazeDataExists` abort path)
  - [ ] A file with an unrecognized column schema (to exercise/confirm the
    `detectImportSourceType` failure path is a clean, informative error)
- [ ] Unit tests per pipeline stage: `interpolateGaze`, `eyeSelection` (all 4 methods:
  Maximize/Strict/Left/Right), `classifyBlinks`, `classifyOffscreenGaze`,
  `calculateVisualAngle`, `calculateVelocity_va_ms`, `classifyGazeIVT`,
  `mergeAdjacentFixation`, `removeShortFixations`.
- [ ] Integration test: full `eyeQuality()` run end-to-end on each fixture, snapshot-testing
  key output columns and `calculateOutputMetrics()` summary values.
- [ ] Batch test: `eyeQualityBatch()` against a small synthetic BIDS directory (2 subjects ×
  2 sessions), verifying the correct number of output files, correct `numcores` logic at
  edge cases (1 file, 0 cores available), and that `parsePreprocessingBatchSummary()` round-trips.
- [ ] Regression test specifically for the hardcoded-display-dimension bug (A.1) — pass
  non-default dims through `eyeQualityBatch()` and assert they reach `calculateVisualAngle()`.
- [ ] Test `create_new_filename()` path construction on Windows-style and POSIX-style paths.
- [ ] Add `testthat::skip_on_cran()` around anything slow/parallel; keep CRAN-run tests fast
  (CRAN checks should complete in well under a minute for a package this size).
- [ ] Target/measure test coverage with `covr::package_coverage()`; aim for meaningful
  coverage on the seam files identified in Section A/E before declaring "release ready."

---

## D. Documentation checklist

- [ ] Add `vignettes/getting-started.Rmd` — single-file walkthrough (already have most of the
  content in README, needs conversion + a runnable example against a bundled fixture).
- [ ] Add `vignettes/batch-processing.Rmd` — BIDS-like directory walkthrough.
- [ ] Add `vignettes/output-data-dictionary.Rmd` (or keep as a README table, but a searchable
  vignette version is more discoverable) — expand the existing table with units and one-line
  "why this matters for QC" notes.
- [ ] **New, given today's discussion:** `vignettes/adding-a-new-eye-tracker.Rmd` — a
  contributor guide for extending device support (see Section E). This is likely to be the
  single highest-leverage doc for community adoption.
- [ ] Document the `maxValidityThreshold` parameter's device-specific meaning much more
  clearly — currently the roxygen doc says "Only applicable if software = TobiiStudio" but
  the parameter is always exposed on `eyeQuality()`, which is confusing for Tobii Pro users.
- [ ] Fix the dead link in README's "Contributing" section: `open an issue on github` points to
  `elab-umn/eyetrackingELabR/issues` — wrong repo name, should be `elab-umn/eyeQuality/issues`.
- [ ] Update installation section once on CRAN (`install.packages("eyeQuality")`), keep
  GitHub dev-version instructions as secondary.
- [ ] Regenerate `README.md` from `README.Rmd` after all example code changes (confirm current
  `.Rbuildignore` correctly excludes `README.Rmd` from the built package but that CI/pre-commit
  regenerates `README.md` from it — currently no evidence of a Makefile/GH Action doing this).
- [ ] Add a `CONTRIBUTING.md` (currently contributing instructions live only inside README) —
  particularly important once you're inviting external device-adapter contributions.
- [ ] Add a `CODE_OF_CONDUCT.md` if you want external contributors (common expectation for
  community-extensible packages).

---

## E. MAJOR: Generalizing beyond Tobii to arbitrary eye trackers

This is the part worth treating as its own mini-project/paper, not a bullet in the CRAN
checklist. Below is what I found by tracing every device-specific branch point in the code,
plus the architecture changes needed to make "add a new eye tracker" a config/contribution
task instead of a core-code-editing task.

### E.1 — Exactly where device-specific logic currently lives (confirmed by grep)

| File | What's hardcoded | Generalization needed |
|---|---|---|
| `detectImportSourceType.R` | Detects only `StudioVersionRec` / `Recording software version` columns | Pluggable detector registry |
| `standardizeColumnNames.R` / `renameColumns.R` | Literal Tobii column-name → standard-name mapping, two `if/else` branches | Config-driven mapping (YAML/JSON per device), or user-supplied mapping function |
| `extractEventRows.R` | Tobii-specific "how do I tell event rows from gaze rows" logic (`Sensor == "Eye Tracker"` vs. `eyeTrackerTimestamp != -9999`) | Generalized event/gaze separation rule per device, with a sensible default (e.g., "no dedicated event stream") for devices that don't interleave events in the same file |
| `removeInvalidGaze.R` | Two incompatible validity models: TobiiPro's categorical `"Valid"/"Invalid"` string vs. TobiiStudio's numeric `0–4` threshold | Common **confidence/validity normalization layer** — every adapter maps its native validity representation to a single internal scale (see E.4) |
| `calculateVisualAngle.R` / the `distanceZ` requirement throughout `eyeQuality.R` | Assumes a **remote, screen-based** tracker with known screen distance, resolution, and physical screen dimensions | Needs a **pluggable geometry model** — screen-based visual-angle math is not meaningful for head-mounted trackers (see E.3) |
| `eyeQuality.R` `maxValidityThreshold` param | Threshold semantics only make sense for TobiiStudio's 0–4 scale | Should become part of each device adapter's config, not a pipeline-level argument |

**The good news, confirmed by code review:** everything *downstream* of standardized
columns — `eyeSelection.R`, `classifyBlinks.R`, `calculateFrequency_hz.R`,
`interpolateGaze.R`, `smoothGaze.R`, `classifyGazeIVT.R`, `calculateOutputMetrics.R` — is
already written against generic column names (`gazeLeftX`, `pupilLeft`, etc.) with no
device branching. **The refactor is concentrated in ~5 files at the front of the pipeline
plus the visual-angle/geometry stage**, not spread throughout.

### E.2 — Target architecture: adapter/plugin pattern

- [ ] Define a **common intermediate schema** (formalize what's implicitly already the
  standard: `event`, `eventValue`, `recordingTimestamp_ms`, `gazeLeftX/Y`, `gazeRightX/Y`,
  `pupilLeft/Right`, `distanceLeftZ/RightZ`, `validityLeft/Right` — document as an actual
  data dictionary with types/units/allowed-NA semantics, not just inferred from code).
- [ ] Add a `confidence` (0–1 continuous) column to the schema alongside/instead of
  `validityLeft/Right`, so binary, 5-point, and continuous validity models all normalize
  into the same downstream representation.
- [ ] Replace `detectImportSourceType()` + `standardizeColumnNames()` with a **device adapter
  registry**: each adapter is a small S3/S4 object or list with `detect(data)`,
  `standardize(data)`, `extract_events(data)`, `normalize_validity(data)`, and (new)
  `geometry_type` (`"screen"` vs `"headmounted"` vs `"none"`).
- [ ] Ship built-in adapters for the two existing formats (`tobii_studio`, `tobii_pro`) as
  the reference implementation — this is a good forcing function to also fix the bugs in
  Section A, since you'll be rewriting these files anyway.
- [ ] Design and document `register_eyetracker_adapter()` (or similar) as the public
  extension point — this is what turns "add a new eye tracker" into a contribution PR
  instead of a core-logic change, and is the actual mechanism behind the "generalizes to
  any eye tracker" claim.
- [ ] Decide packaging: should third-party adapters live in `eyeQuality` itself (curated,
  slower-moving, CRAN-gated) or should `eyeQuality` support **externally registered
  adapters** from companion packages (e.g., a hypothetical `eyeQuality.pupillabs`)? I'd
  lean toward: core ships Tobii Studio/Pro + Pupil Labs (three concrete implementations
  needed to prove the abstraction is actually general, per the "rule of three"), with the
  registry open for others.

### E.3 — Screen-based vs. head-mounted geometry (the hardest real problem here)

- [ ] Formally split "visual angle from screen geometry" out of the main pipeline as an
  **optional** stage, gated by adapter `geometry_type`.
- [ ] For head-mounted trackers (Pupil Labs Core/Neon, and similarly SMI/others), gaze is
  typically reported as normalized scene-camera coordinates (`norm_pos_x/y` in [0,1]) or a
  3D gaze direction vector — there is no "distance from screen" in the Tobii sense unless
  the wearer is looking at a fixed screen and you've done additional AOI/surface mapping.
  Decide scope for v1:
  - **Option A (recommended for v1):** support head-mounted **data quality metrics**
    (missingness, confidence-based validity, blink detection, sampling-rate checks,
    fixation/saccade classification via angular velocity in scene-camera space) without
    requiring `distanceZ`/physical screen mm — i.e., make `displayDimensionX_mm`,
    `displayDimensionY_mm`, `distanceZ` **optional**, and skip pixel↔visual-angle conversion
    when they're not supplied or not applicable, falling back to normalized/angular units.
  - **Option B (larger scope, v2+):** full surface/AOI mapping (Pupil Labs' own "Surface
    Tracker" concept) to project head-mounted gaze onto a defined screen/object plane and
    compute true visual angle — this is a substantial feature in its own right and I'd
    treat it as a separate roadmap item, not a blocker for the initial generalization.
- [ ] Re-derive `classifyGazeIVT()`'s velocity-threshold approach for angular/scene-camera
  coordinates (I-VT is a general algorithm, not screen-specific — the input units just need
  to be angular velocity regardless of source, which head-mounted normalized coordinates
  can approximate with a known camera FOV).
- [ ] Monocular-by-design devices (some head-mounted setups only track one eye, or Pupil
  Labs Core historically ran monocular in some configs) — confirm `eyeSelection.R`'s
  "Maximize/Strict/Left/Right" methods degrade sensibly when one eye's columns are
  entirely absent (not just NA) rather than assuming both columns always exist.

### E.4 — Validity/confidence normalization

- [ ] Define the common internal scale (recommend continuous `0–1` "confidence", with
  each adapter responsible for mapping its native representation):
  - Tobii Studio: `0–4` discrete → invert/rescale to `0–1`, or keep discrete but document
    threshold semantics per adapter
  - Tobii Pro: `"Valid"/"Invalid"` categorical → `1.0`/`0.0`
  - Pupil Labs: native `confidence` field is *already* `0–1` continuous — arguably the
    easiest adapter to write, and a good first non-Tobii proof of concept
- [ ] Make `removeInvalidGaze()`'s threshold logic operate on the normalized scale, not on
  device-specific magic numbers, and expose the threshold as an adapter default that users
  can override.

### E.5 — Practical rollout plan for generalization (sequenced)

1. [ ] Refactor existing Tobii Studio/Pro logic behind the new adapter interface **with no
     behavior change** (this is really Section A's bug-fix work reframed as a refactor —
     good opportunity to fix `extractEventRows.R`'s missing-else bug and
     `removeInvalidGaze.R`'s FIXME as part of the same pass).
2. [ ] Add full regression tests (Section C) *before* this refactor lands, so you can prove
     zero behavior change on existing Tobii data.
3. [ ] Write the Pupil Labs adapter as the second/proof implementation — start with Option A
     scope (no surface mapping), since Pupil Labs' open CSV/JSON export format
     (`gaze_positions.csv`, `pupil_positions.csv`, `blinks.csv`) is well documented and
     would validate the abstraction without requiring you to solve AOI mapping first.
4. [ ] Write `vignettes/adding-a-new-eye-tracker.Rmd` using the Pupil Labs adapter as the
     worked example.
5. [ ] Publish the adapter registry as a clearly-marked "extension point" in package docs —
     this, more than any Shiny app, is what would make this a genuine field contribution:
     a shared, reproducible, adapter-based QC standard that other labs can plug their own
     device into rather than reinventing preprocessing per-lab (which is the current status
     quo across most eye-tracking research code I'd expect you're familiar with).
6. [ ] Consider whether this is worth its own methods paper (JOSS — Journal of Open Source
     Software — is a natural fit for a package like this once the adapter architecture and
     ≥2 device implementations exist; it's specifically designed for exactly this kind of
     "well-engineered open research software" contribution).

---

## F. Batch processing & directory flexibility checklist
*(carried over/expanded from prior discussion, included here for a single source of truth)*

- [ ] Fix hardcoded display dims in `eyeQualityBatch.R` (A.1).
- [ ] Add `outputDir` parameter threaded through `eyeQuality()` → `saveFiles()` →
  `create_new_filename()`, defaulting to current behavior.
- [ ] Generalize `listBidsFiles()` beyond fixed two-level `sub-*/ses-*`: support arbitrary
  depth and/or a user-supplied glob/path-template.
- [ ] Add a `batch_config.yaml` schema (directory, device/adapter type, display dims if
  applicable, eye selection method, validity/confidence threshold, output dir, subject/session
  patterns) as the single source of config truth for both CLI and the future Shiny setup app.
- [ ] Add resumability to `eyeQualityBatch()`: skip files that already have a matching
  `qcsummary` output for the given `batchName`, with a `force = TRUE` override.
- [ ] Surface batch-level failure detail better — currently failures are inferred by diffing
  attempted vs. `qcsummary`-produced files; capture and persist the actual error/traceback per
  file (already caught in `tryCatch` but only printed, not saved structured).
- [ ] Finish `parsePreprocessingBatchSummary()`'s `failedfiles`/`successfulfiles` branches (A.4).

---

## G. Shiny app checklists

### G.1 — Setup & Run app
- [ ] Directory picker with live dry-run preview (call `listBidsFiles()` before committing,
  show matched file count + first N filenames)
- [ ] Parameter form mapped 1:1 to `batch_config.yaml` fields
- [ ] Device/adapter selector (once E is implemented) instead of assuming Tobii
- [ ] Save/load named configs for repeat runs
- [ ] Background execution strategy decided (in-process `future`/`promises` vs. spawned
  `processx` Rscript) — needed before progress streaming can work
- [ ] Live progress (files completed / failed / remaining) + tail of run log
- [ ] Post-run summary screen linking into the Analyze app

### G.2 — Analyze/QC Explorer app
- [ ] Load one or more `qcsummary.tsv` outputs into a sortable/filterable table
- [ ] Configurable QC thresholds (missingness %, robustness) with visual flagging
- [ ] Row click → render that file's three plots via `generateEyeTrackingPlots()`
- [ ] Cross-file comparison view (distribution of a chosen QC metric across the batch)
- [ ] Export "flagged for review" file list
- [ ] (Post-E) Adapter-aware display — head-mounted files won't have all the same QC
  columns as screen-based ones; table/plot logic needs to degrade gracefully

---

## H. Suggested execution order (dependency-aware, not effort-ordered)

1. A (bugs) — cheap, and several block correct behavior of everything else
2. C (tests) — must exist *before* the E refactor to prove no regressions
3. E.2/E.5 steps 1–2 (adapter refactor of existing Tobii logic, zero behavior change)
4. B (CRAN mechanics) — can run in parallel with the above once A/C are stable
5. F (batch/directory flexibility)
6. E.5 steps 3–6 (Pupil Labs adapter + generalization docs) — the "major contribution" piece
7. G.1, then G.2 (Shiny apps) — deliberately last, since they're thin layers over an engine
   that needs to be correct and adapter-aware first, or you'll be rebuilding the GUI twice
8. Final CRAN submission pass
