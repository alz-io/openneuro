test_that("get_metadata returns expected structure", {
  skip_if_offline()
  skip_on_cran()

  meta <- get_metadata("ds000001")

  expect_type(meta, "list")
  expect_equal(meta$id, "ds000001")
  expect_true(is.character(meta$title))
  expect_true(is.character(meta$version))
  expect_type(meta$n_subjects, "integer")
  expect_gte(meta$n_subjects, 1)
  expect_type(meta$modalities, "list")
  expect_type(meta$tasks, "list")
  expect_true(length(meta$modalities) > 0)
  expect_true(length(meta$tasks) > 0)
  expect_true(is.logical(meta$public))
  expect_true(meta$public)
})

test_that("get_metadata errors on invalid ID", {
  expect_error(get_metadata("invalid"), "must start with")
})

test_that("get_metadata errors on missing ID", {
  expect_error(get_metadata())
})
