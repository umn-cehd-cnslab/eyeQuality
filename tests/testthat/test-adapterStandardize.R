# P3-04 follow-up: regression coverage for the new adapter standardize()
# implementations (.tobii_studio_standardize() in R/tobii-studio-adapter.R,
# .tobii_pro_standardize() in R/tobii-pro-adapter.R), which were ported
# unchanged from the pre-Phase-3 renameColumns() TobiiStudio/TobiiPro
# branches (R/renameColumns.R). Both the old path
# (standardizeColumnNames() -> renameColumns()) and the new path
# (registered_adapters()[["TobiiStudio"/"TobiiPro"]]$standardize()) are
# still live in the codebase as of P3-04 -- the actual eyeQuality() pipeline
# still calls the old path directly and isn't rewired to the adapter
# registry until P3-07. Until then, these tests assert the two paths are
# byte-for-byte equivalent on all 4 Tobii fixtures, locking in the
# behavioral-equivalence claim from the P3-04 manual comparison.
#
# NOTE: this test file is deliberately temporary-shaped. Once P3-07 removes
# the old renameColumns()/standardizeColumnNames() code path, this old-vs-new
# comparison will no longer make sense -- at that point these tests should be
# simplified to assert the adapter standardize() output against known
# expected values directly, rather than against the (removed) old path.

test_that("TobiiStudio adapter standardize() matches old renameColumns() path on the sample fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  data <- importData(fp)

  old_result <- standardizeColumnNames(data, "TobiiStudio")
  new_result <- registered_adapters()[["TobiiStudio"]]$standardize(data)

  expect_identical(names(old_result), names(new_result))
  expect_identical(vapply(old_result, class, character(1)), vapply(new_result, class, character(1)))
  expect_equal(old_result, new_result)
})

test_that("TobiiStudio adapter standardize() matches old renameColumns() path on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  data <- importData(fp)

  old_result <- standardizeColumnNames(data, "TobiiStudio")
  new_result <- registered_adapters()[["TobiiStudio"]]$standardize(data)

  expect_identical(names(old_result), names(new_result))
  expect_identical(vapply(old_result, class, character(1)), vapply(new_result, class, character(1)))
  expect_equal(old_result, new_result)
})

test_that("TobiiPro adapter standardize() matches old renameColumns() path on the sample fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_sample.tsv")
  data <- importData(fp)

  old_result <- standardizeColumnNames(data, "TobiiPro")
  new_result <- registered_adapters()[["TobiiPro"]]$standardize(data)

  expect_identical(names(old_result), names(new_result))
  expect_identical(vapply(old_result, class, character(1)), vapply(new_result, class, character(1)))
  expect_equal(old_result, new_result)
})

test_that("TobiiPro adapter standardize() matches old renameColumns() path on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)

  old_result <- standardizeColumnNames(data, "TobiiPro")
  new_result <- registered_adapters()[["TobiiPro"]]$standardize(data)

  expect_identical(names(old_result), names(new_result))
  expect_identical(vapply(old_result, class, character(1)), vapply(new_result, class, character(1)))
  expect_equal(old_result, new_result)
})
