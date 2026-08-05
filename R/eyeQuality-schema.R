#' The eyeQuality common intermediate schema
#'
#' @description
#' Every device adapter's `standardize()`, `extract_events()`, and
#' `normalize_validity()` methods must produce data conforming to this
#' schema before handing off to the generic (device-agnostic) pipeline —
#' `eyeSelection()`, `classifyBlinks()`, `calculateFrequency_hz()`,
#' `interpolateGaze()`, `smoothGaze()`, `classifyGazeIVT()`,
#' `calculateOutputMetrics()`, etc. Those downstream functions are written
#' entirely against the generic column names documented here and contain no
#' device-specific logic; this file is the contract that makes that possible.
#'
#' This topic documents no callable function — it exists purely as a
#' reference target (`?eyeQuality-schema`). It is also the source of truth
#' for the `output-data-dictionary.Rmd` vignette: that vignette should be
#' generated from (or kept in lockstep with) the table below rather than
#' re-deriving column semantics from pipeline source a second time.
#'
#' # Column reference
#'
#' Each entry below lists: type, units, NA semantics, and which pipeline
#' stage first produces / primarily consumes the column.
#'
#' ## `event`
#' - **Type:** character
#' - **Units:** n/a (free-text event label, device- and study-specific)
#' - **NA semantics:** `NA` on every gaze-stream row; non-`NA` only on rows
#'   that represent a logged event (marker, message, stimulus onset, etc.)
#'   rather than a gaze sample. Adapters split raw rows into gaze-stream vs.
#'   event rows using a device-specific discriminator (e.g. Tobii Pro's
#'   `Sensor` column, Tobii Studio's `eyeTrackerTimestamp == -9999` sentinel)
#'   before this split is even meaningful — see `extract_events()` below.
#' - **Produced by:** adapter `extract_events()` (`event` data.frame half of
#'   its `list(gaze, events)` return; also present, all-`NA`, in `gaze`).
#' - **Consumed by:** `setTimestamps()` / `getEventTimes()`, to locate
#'   study-defined start/end markers (`studioEvents`/`proEvents` in
#'   `eyeQuality()`).
#'
#' ## `eventValue`
#' - **Type:** character
#' - **Units:** n/a
#' - **NA semantics:** same pattern as `event` — populated only on event
#'   rows, paired with `event` (label, value) so a single event row can
#'   carry both a category and a payload (e.g. `event = "TrialStart"`,
#'   `eventValue = "trial_014"`).
#' - **Produced by:** adapter `extract_events()`.
#' - **Consumed by:** currently informational/pass-through; not read by any
#'   generic pipeline stage today, but preserved through the pipeline for
#'   downstream analysis and potential future use by `setTimestamps()`.
#'
#' ## `recordingTimestamp_ms`
#' - **Type:** numeric
#' - **Units:** milliseconds, relative to recording start (device-native
#'   clock; adapters do not currently renormalize this to a shared epoch)
#' - **NA semantics:** should not be `NA` for any row that survives
#'   `extract_events()` — every gaze and event row carries a timestamp.
#' - **Produced by:** adapter `standardize()` (renamed from the device's
#'   native recording-timestamp column).
#' - **Consumed by:** `setTimestamps()` (time-window filtering via
#'   `timeStart`/`timeEnd`), `calculateFrequency_hz()`,
#'   `calculateVelocity_va_ms()`, `interpolateGaze()`/`smoothGaze()` (sample
#'   spacing), and retained through to final output.
#'
#' ## `gazeLeftX` / `gazeLeftY` / `gazeRightX` / `gazeRightY`
#' - **Type:** numeric
#' - **Units:** pixels (`px`), in on-screen display coordinate space, for
#'   every adapter that exists as of this writing (`geometry_type ==
#'   "screen"`). A `geometry_type == "headmounted"` adapter (Phase 5) is
#'   expected to populate these with a different native unit (e.g.
#'   normalized scene-camera coordinates) — see the `geometry_type` note
#'   below and P4-06's still-open unit-labeling decision. This schema does
#'   not resolve that decision; it only records that gaze coordinates are
#'   `geometry_type`-dependent in unit, not just in downstream conversion.
#' - **NA semantics:** `NA` = no gaze sample recorded for that eye at that
#'   timestamp (device-reported gap, not yet a validity judgment). Adapters
#'   do not mask these raw columns for validity — masking happens
#'   separately, into new `.valid`-suffixed columns (see
#'   `normalize_validity()` below), so raw values remain traceable through
#'   the whole pipeline.
#' - **Produced by:** adapter `standardize()`.
#' - **Consumed by:** adapter `normalize_validity()` (produces
#'   `gazeLeftX.valid`, etc.), then `removeOffscreenGaze()`,
#'   `interpolateGaze()`, `eyeSelection()` (`.int` → `.eyeSelect`),
#'   `smoothGaze()` (→ `.smooth`), `calculateGaze_va()` (→ `gazeX_va`/
#'   `gazeY_va`), and finally `assignFinalColumnNames()`
#'   (`gazeX.preprocessed_px`, `gazeX.preprocessed_va`).
#'
#' ## `pupilLeft` / `pupilRight`
#' - **Type:** numeric
#' - **Units:** millimeters (`mm`) diameter, for current Tobii adapters.
#' - **NA semantics:** `NA` = no pupil measurement at that timestamp (e.g.
#'   eye closed/blink, tracking loss). Not validity-masked in the raw
#'   column, same rule as the gaze columns above.
#' - **Produced by:** adapter `standardize()`.
#' - **Consumed by:** adapter `normalize_validity()` (→ `pupilLeft.valid`),
#'   `interpolateGaze()` (→ `.int`), `classifyBlinks()` (blink detection is
#'   pupil-signal-driven), `eyeSelection()` (→ `.eyeSelect`), `smoothGaze()`
#'   (→ `.smooth`), `assignFinalColumnNames()` (→ `pupil.preprocessed`).
#'
#' ## `distanceLeftZ` / `distanceRightZ`
#' - **Type:** numeric
#' - **Units:** millimeters (`mm`), eye-to-screen distance along the Z axis,
#'   for current (screen-geometry) Tobii adapters. Not meaningful for a
#'   `geometry_type == "headmounted"` adapter, which has no fixed screen
#'   plane to measure distance to — such an adapter is expected to leave
#'   this column entirely `NA` or omit the underlying measurement concept
#'   (final decision deferred to Phase 4/5, same as the gaze-unit question
#'   above).
#' - **NA semantics:** `NA` = no distance measurement at that timestamp.
#' - **Produced by:** adapter `standardize()`.
#' - **Consumed by:** adapter `normalize_validity()` (→
#'   `distanceLeftZ.valid`), `interpolateGaze()`, `removeOffscreenGaze()`
#'   (distance sanity-checking), `calculateGaze_va()` (visual-angle
#'   conversion requires eye-to-screen distance), `assignFinalColumnNames()`
#'   (→ `distanceZ.preprocessed_mm`).
#'
#' ## `validityLeft` / `validityRight`
#' - **Type:** device-native, deliberately NOT normalized at this stage —
#'   Tobii Studio: numeric, discrete `0`-`4` (`0` = best, `4` = worst,
#'   matching Tobii's own convention); Tobii Pro: character, `"Valid"` /
#'   `"Invalid"`.
#' - **Units:** n/a (categorical/ordinal code).
#' - **NA semantics:** `NA` denotes no validity code reported for that
#'   sample. Current `normalize_validity()`/`removeInvalidGaze()` behavior
#'   does **not** mask `NA` validity — an `NA` validity code passes through
#'   **unmasked** (i.e. that sample is treated as if it were valid), for
#'   both adapters, though for different mechanical reasons rather than by
#'   deliberate design:
#'   - Tobii Pro: `replaceRows <- (validityCol == "Invalid")` is `NA` when
#'     `validityCol` is `NA`; `tidyr::replace_na(replaceRows, FALSE)` then
#'     coerces that to `FALSE`, so the sample is not masked.
#'   - Tobii Studio: `data[[newCol]][validityVals > threshold] <- NA` — the
#'     comparison is `NA` when `validityVals` is `NA`, and R's
#'     subset-assignment `x[NA] <- value` is a no-op at `NA`-indexed
#'     positions (verified: `x <- c(1,2,3,4); x[c(FALSE,NA,TRUE,TRUE)] <-
#'     NA` leaves position 2 as `2`, not `NA`), so the sample is likewise
#'     left unmasked.
#'   This is a pre-existing quirk of the current pipeline (arguably a
#'   latent correctness gap — "unknown validity" arguably *should* mean
#'   "don't trust this sample," not "assume it's fine") that P3-01 is
#'   documenting as-is, not fixing. See the `confidenceLeft`/`confidenceRight`
#'   columns below for how this quirk interacts with the new schema, and for
#'   the resolved `NA`-validity mapping decision.
#' - **Produced by:** adapter `standardize()` (renamed, but not
#'   reinterpreted, from the device's native validity/eye-openness column).
#' - **Consumed by:** adapter `normalize_validity()` only — this is
#'   intentionally the sole consumer. Every downstream pipeline stage reads
#'   the generic, cross-device `confidenceLeft`/`confidenceRight` columns
#'   instead (see below), so that adding a future adapter with a wholly
#'   different validity representation (e.g. a head-mounted tracker's
#'   continuous confidence score) requires no changes outside that
#'   adapter's own `normalize_validity()`.
#'
#' ## `confidenceLeft` / `confidenceRight` (new — introduced by this schema,
#' not present pre-Phase-3)
#' - **Type:** numeric
#' - **Units:** unitless, continuous scale, **`0`-`1`**, where `1` = highest
#'   confidence / most valid, `0` = lowest confidence / definitely invalid.
#'   This is the reverse convention from Tobii Studio's native `0`-`4`
#'   validity codes (where `0` is best) — the mapping deliberately flips
#'   direction so that "higher is better" holds universally across every
#'   current and future adapter, rather than perpetuating a device-specific
#'   convention into the generic pipeline.
#' - **Split by eye, not a single combined column:** P3-06's implementation
#'   emits `confidenceLeft`/`confidenceRight` rather than a single
#'   `confidence` value, following every other per-eye pair already in this
#'   schema (`gazeLeftX`/`gazeRightX`, `pupilLeft`/`pupilRight`,
#'   `validityLeft`/`validityRight`, `distanceLeftZ`/`distanceRightZ`). A
#'   single unsplit `confidence` column was underspecified from the start —
#'   it left unstated whether/how a per-eye validity code should be combined
#'   into one number (min? mean? which eye wins?) — and every current source
#'   column this is derived from is already per-eye, so per-eye output is
#'   the natural and consistent shape. Anything needing a single combined
#'   confidence value (e.g. eye-selection logic, Phase 5+) can derive it
#'   from `confidenceLeft`/`confidenceRight` at the point of use, the same
#'   way `eyeSelection()` already derives single `gazeX`/`gazeY` from
#'   `gazeLeftX`/`gazeRightX`.
#' - **Design rationale (why a 0-1 continuous scale, not a re-exported
#'   discrete code):** the two current Tobii sources use structurally
#'   different validity representations (a 5-level ordinal code vs. a
#'   binary string), and a future head-mounted adapter (Phase 5) is
#'   expected to report a genuinely continuous, non-Tobii-native confidence
#'   estimate (e.g. pupil-detection confidence from a computer-vision
#'   model). A discrete or Tobii-shaped intermediate representation would
#'   force every future adapter to either lossily bucket a continuous score
#'   into Tobii's 5 levels, or bypass the shared column entirely — both
#'   defeat the point of a common schema. A continuous `0`-`1` scale is the
#'   smallest common denominator: any discrete/ordinal code maps onto it
#'   losslessly (evenly-spaced buckets), any binary flag maps onto it
#'   trivially (`0`/`1`), and any genuinely continuous confidence score
#'   (Phase 5) needs no lossy transformation at all to populate it.
#' - **Mapping used by the current Tobii adapters** (applied independently
#'   to each eye, `validityLeft -> confidenceLeft`, `validityRight ->
#'   confidenceRight`):
#'   - Tobii Studio (`validityLeft`/`validityRight` `0`-`4`):
#'     `confidence<Eye> <- 1 - (validity<Eye> / 4)`, i.e. `0 -> 1.0`,
#'     `1 -> 0.75`, `2 -> 0.5`, `3 -> 0.25`, `4 -> 0.0`. The existing
#'     `maxValidityThreshold` default of `2` (P3-02's `default_thresholds`)
#'     therefore corresponds to a `confidence<Eye>` cutoff of `0.5` on this
#'     scale — masking (`.valid` column set to `NA`) still happens at
#'     `validity > threshold`, matching current behavior exactly;
#'     `confidence<Eye>` is an additional, informational column, not (yet)
#'     the thing masking is computed from, so this is zero behavior change
#'     for P3-06. Implemented in `R/tobii-studio-adapter.R`'s
#'     `.tobii_studio_confidence()`, called from `normalize_validity()`.
#'   - Tobii Pro (`validityLeft`/`validityRight` `"Valid"`/`"Invalid"`):
#'     `confidence<Eye> <- ifelse(validity<Eye> == "Valid", 1, 0)`.
#'     Implemented in `R/tobii-pro-adapter.R`'s `.tobii_pro_confidence()`,
#'     called from `normalize_validity()`.
#'   - A future head-mounted/Pupil Labs-style adapter (Phase 5) is expected
#'     to populate confidence directly from its own native, already-
#'     continuous confidence/quality score, with no discretization step.
#' - **NA semantics — RESOLVED (P3-06):** as documented above under
#'   `validityLeft`/`validityRight`, the *current* `.valid`-masking behavior
#'   treats `NA` validity as unmasked, i.e. as if it were valid. That
#'   masking behavior itself was in-scope for Phase 3's zero-behavior-change
#'   gate and did not change. For the new `confidenceLeft`/`confidenceRight`
#'   columns, which have no pre-Phase-3 behavior to preserve, P3-06 chose:
#'   - **`confidence<Eye> = 1` for `NA` validity**, for both adapters. This
#'     preserves the existing masking quirk's *effective* semantics
#'     (NA-validity samples are, today, treated as valid/kept), so
#'     `confidence<Eye>` stays consistent with what `.valid`-masking
#'     actually does right now, rather than introducing a second,
#'     disagreeing notion of validity for the same input. Since nothing
#'     downstream reads `confidence<Eye>` yet (see "Consumed by" below),
#'     internal consistency with `.valid`-masking was judged the more
#'     conservative choice for a Phase 3 refactor whose explicit goal is
#'     zero behavior change; the rejected alternative (`confidence<Eye> = 0`,
#'     treating unknown validity as low-confidence) would have introduced a
#'     disagreement between `confidence<Eye>` and `.valid`-masking on
#'     `NA`-validity rows with no compensating benefit today.
#'   - Implemented identically on both adapters: `R/tobii-studio-adapter.R`'s
#'     `.tobii_studio_confidence()` and `R/tobii-pro-adapter.R`'s
#'     `.tobii_pro_confidence()`, both called from their respective
#'     `normalize_validity()` (`.tobii_studio_norm_validity()` /
#'     `.tobii_pro_normalize_validity()`).
#' - **Produced by:** adapter `normalize_validity()`, alongside (not
#'   instead of) the `.valid`-suffixed masked columns described below.
#' - **Consumed by:** intended for QC reporting / `calculateOutputMetrics()`
#'   and any future threshold-based filtering keyed on the
#'   adapter-independent `validityThreshold` argument (P3-08). As of P3-06
#'   these columns are produced by both Tobii adapters but not yet read by
#'   any downstream generic pipeline stage.
#'
#' # Derived / suffixed columns (not raw schema fields, but load-bearing)
#'
#' The schema above describes what an adapter must produce. The generic
#' pipeline then derives further columns from it using a consistent suffix
#' convention — documented here because P3-06 in particular introduces the
#' first of these (`.valid`) at the adapter boundary rather than deeper in
#' the pipeline:
#'
#' - **`<col>.valid`** — raw column value, `NA`-masked at invalid/
#'   below-threshold samples; raw `<col>` itself is left byte-identical.
#'   Produced by adapter `normalize_validity()`. Applies to `gazeLeftX/Y`,
#'   `gazeRightX/Y`, `pupilLeft/Right`, `distanceLeftZ/RightZ`.
#' - **`<col>.int`** — interpolated (gap-filled) version of a `.valid`
#'   column. Produced by `interpolateGaze()`.
#' - **`<col>.eyeSelect`** — per-eye columns collapsed to a single
#'   `gazeX`/`gazeY`/`distanceZ`/`pupil` series per the chosen
#'   `eyeSelection_method`. Produced by `eyeSelection()`.
#' - **`<col>.smooth`** — smoothed (denoised) version. Produced by
#'   `smoothGaze()`/`smoothVelocity()`.
#' - **`<col>_va`** — visual-angle-converted gaze coordinate (degrees).
#'   Produced by `calculateGaze_va()`.
#' - **`<col>.preprocessed_px` / `_mm` / `_va` / `_va_ms`** — final output
#'   columns. Produced by `assignFinalColumnNames()`.
#'
#' # `geometry_type` and the schema
#'
#' Every adapter declares `geometry_type` (`"screen"` | `"headmounted"` |
#' `"none"`, see `R/adapter-interface.R`, P3-02). All Tobii adapters as of
#' Phase 3 are `"screen"`; the schema above (`px` gaze units, `mm`
#' distances, screen-relative visual angle) reflects that. `geometry_type`
#' is what will gate whether pixel/visual-angle conversion and distance-
#' based logic run at all for a future `"headmounted"` adapter — the schema
#' itself does not change shape, but the unit/semantics of `gazeLeftX/Y`
#' and `distanceLeftZ/RightZ` for a non-screen adapter are an explicitly
#' open question (Phase 4/P4-06), not resolved here.
#'
#' @name eyeQuality-schema
#' @keywords internal
NULL
