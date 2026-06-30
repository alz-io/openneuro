test_that("on_check_dataset_id validates correctly", {
  expect_error(openneuro:::on_check_dataset_id("ds000001"), NA)
  expect_error(openneuro:::on_check_dataset_id("ds123456"), NA)
  expect_error(openneuro:::on_check_dataset_id("invalid"), "must start with")
  expect_error(openneuro:::on_check_dataset_id("ds00001"), "must start with")
  expect_error(openneuro:::on_check_dataset_id("ds000001a"), "must start with")
})

test_that("on_parse_bids_filename parses standard entities", {
  parsed <- openneuro:::on_parse_bids_filename(
    "sub-01/ses-1/func/sub-01_ses-1_task-rest_acq-fullbrain_run-1_bold.nii.gz"
  )
  expect_equal(parsed$subject, "01")
  expect_equal(parsed$session, "1")
  expect_equal(parsed$task, "rest")
  expect_equal(parsed$acquisition, "fullbrain")
  expect_equal(parsed$run, "1")
  expect_equal(parsed$suffix, "bold")
})

test_that("on_parse_bids_filename handles top-level files", {
  parsed <- openneuro:::on_parse_bids_filename("dataset_description.json")
  expect_true(is.na(parsed$subject))
  expect_equal(parsed$suffix, "dataset_description")
  expect_equal(parsed$extension, ".json")

  parsed <- openneuro:::on_parse_bids_filename("participants.tsv")
  expect_equal(parsed$suffix, "participants")
  expect_equal(parsed$extension, ".tsv")
})

test_that("on_get_url returns expected URL", {
  expect_equal(openneuro:::on_get_url(), "https://openneuro.org/crn/graphql")
})
