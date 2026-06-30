test_that("bids_index parses filenames correctly", {
  filenames <- c(
    "sub-01/anat/sub-01_T1w.nii.gz",
    "sub-01/func/sub-01_task-rest_run-01_bold.nii.gz",
    "sub-02/ses-1/func/sub-02_ses-1_task-motor_run-02_events.tsv",
    "participants.tsv",
    "dataset_description.json"
  )

  idx <- bids_index(filenames)
  expect_s3_class(idx, "tbl_df")
  expect_equal(nrow(idx), 5)

  expect_equal(idx$subject[1], "01")
  expect_equal(idx$task[2], "rest")
  expect_equal(idx$run[2], 1L)
  expect_equal(idx$suffix[3], "events")
  expect_equal(idx$extension[3], ".tsv")
  expect_equal(idx$session[3], "1")

  expect_true(is.na(idx$subject[4]))
  expect_equal(idx$suffix[5], "dataset_description")
})

test_that("bids_index works with data frame input", {
  fls <- data.frame(
    filename = c("sub-01/func/sub-01_task-stroop_bold.nii.gz",
                 "sub-01/func/sub-01_task-stroop_events.tsv"),
    size = c(1000, 200),
    stringsAsFactors = FALSE
  )

  idx <- bids_index(fls)
  expect_equal(nrow(idx), 2)
  expect_true("size" %in% names(idx))
})

test_that("bids_index handles empty input", {
  expect_s3_class(bids_index(character(0)), "tbl_df")
  expect_equal(nrow(bids_index(character(0))), 0)
})

test_that("bids_index errors on invalid input", {
  expect_error(bids_index(42), "must be a data frame or character vector")
})
