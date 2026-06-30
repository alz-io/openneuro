test_that("openneuro_cache returns cache path", {
  cachedir <- file.path(tempdir(), "openneuro_cache_test")
  on.exit(unlink(cachedir, recursive = TRUE), add = TRUE)

  result <- openneuro_cache(path = cachedir, clear = TRUE)
  expect_true(dir.exists(result))
  expect_equal(normalizePath(result), normalizePath(cachedir))
})

test_that("openneuro_cache info doesn't error", {
  cachedir <- file.path(tempdir(), "openneuro_cache_info")
  on.exit(unlink(cachedir, recursive = TRUE), add = TRUE)
  dir.create(cachedir, recursive = TRUE)

  expect_error(openneuro_cache(path = cachedir, info = TRUE), NA)
})
