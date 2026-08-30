# eyeQuality 0.1.0

Initial CRAN release.

## New features

* Device-specific import logic has been rewritten around a pluggable
  adapter/registry architecture (`R/adapter-interface.R`). Support for a new
  eye tracker is added by registering an adapter (`register_eyetracker_adapter()`)
  rather than modifying the core pipeline, and `detectImportSourceType()` now
  dispatches against the registry (`registered_adapters()`) instead of
  hardcoded per-format checks. Adapters for Tobii Studio and Tobii Pro exports
  ship with the package.

* `listBidsFiles()` now supports a `layout` argument (`"bids"` or `"glob"`).
  The original fixed two-level BIDS-style hierarchy remains the default;
  `layout = "glob"` adds support for paired-in-place and centralized-output
  directory structures via glob-style `pathPattern`/`excludePattern_regex`
  matching.

* `eyeQualityBatch()` is now resumable: by default (`force = FALSE`), files
  that already have a corresponding QC summary output for the given
  `batchName`/`outputDir` are skipped on a re-run, so an interrupted or
  partially failed batch can be restarted without reprocessing completed
  files. Pass `force = TRUE` to reprocess everything unconditionally, as
  before. `outputDir` is now an explicit, documented argument.

* Per-file batch failures are captured and reported individually instead of
  being discarded. `parsePreprocessingBatchSummary(info_to_extract =
  "failedfiles")` now returns a tibble with `file` and `error` columns (the
  underlying error message for each failed file), and a corresponding
  `"skippedfiles"` section/branch reports files skipped by the new
  resumability behavior.

* Added two vignettes: `vignette("getting-started")`, covering a single-file
  `eyeQuality()` run end to end, and `vignette("batch-processing")`, covering
  both `listBidsFiles()` layouts, resumability, and reading back per-file
  failure detail.

## Breaking changes

* The `maxValidityThreshold` argument to `eyeQuality()` has been removed and
  replaced with `validityThreshold`. `maxValidityThreshold` was expressed on
  Tobii Studio's native 0-4 validity-code scale; `validityThreshold` is
  expressed on the common 0-1 confidence scale used throughout the package
  and is resolved per-adapter, so its meaning is consistent across eye
  tracker types rather than tied to one manufacturer's native encoding.
  Calls that relied on the previous default are unaffected (the default
  behavior is unchanged). Calls that explicitly passed `maxValidityThreshold`
  should switch to `validityThreshold`; see `?eyeQuality` for details.
