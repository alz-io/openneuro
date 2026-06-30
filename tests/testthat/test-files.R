test_that("list_files returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  files <- list_files("ds000001", recursive = FALSE)
  expect_s3_class(files, "tbl_df")
  expect_true(all(c("id", "filename", "size", "directory",
                    "annexed", "url") %in% names(files)))
  expect_true(any(files$directory))
})

test_that("list_files recursive returns files with full paths", {
  skip_if_offline()
  skip_on_cran()

  files <- list_files("ds000001", recursive = TRUE)
  expect_s3_class(files, "tbl_df")
  expect_gte(nrow(files), 10)
  expect_true(any(grepl("sub-", files$filename)))
})

test_that("list_files errors on invalid ID", {
  expect_error(list_files("invalid"), "must start with")
})

test_that("download_files works with filtered file list", {
  skip_if_offline()
  skip_on_cran()
  skip_if(!capabilities("libcurl"))

  files <- list_files("ds000001", recursive = TRUE)
  small_files <- files[!files$directory & files$size < 5000, ]
  if (nrow(small_files) == 0) skip("No small files found")

  test_dir <- file.path(tempdir(), "openneuro_test_dl")
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  result <- download_files(head(small_files, 1), dest_dir = test_dir,
                           dataset_id = "ds000001", quiet = TRUE)

  expect_true(file.exists(result[1]))
})

test_that("download_dataset downloads subject-specific files", {
  skip_if_offline()
  skip_on_cran()
  skip_if(!capabilities("libcurl"))

  test_dir <- file.path(tempdir(), "openneuro_test_ds")
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  result <- download_dataset("ds000001", subjects = "01",
                             dest_dir = test_dir, quiet = TRUE)

  expect_true(length(result) > 0)
  expect_true(any(grepl("sub-01", result)))
})
