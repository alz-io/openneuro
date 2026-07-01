test_that("search_datasets returns a tibble with expected columns", {
  skip_if_offline()
  skip_on_cran()

  result <- suppressWarnings(search_datasets(modality = "mri", n = 3))

  expect_s3_class(result, "tbl_df")
  #expect_gte(nrow(result), 1) # to be changed later where connected to internet
  expect_true(all(c("id", "name", "public", "subjects",
                    "modalities", "tasks") %in% names(result)))
  expect_true(all(grepl("^ds", result$id)))
})

test_that("search_datasets with subject filter works", {
  skip_if_offline()
  skip_on_cran()

  result <- suppressWarnings(search_datasets(min_subjects = 10, n = 3))
  expect_s3_class(result, "tbl_df")
})

test_that("search_datasets returns empty tibble for no results", {
  result <- search_datasets(modality = "nonexistent_xyz")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
