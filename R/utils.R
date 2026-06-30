.openneuro_env <- new.env(parent = emptyenv())

on_get_url <- function() {
  "https://openneuro.org/crn/graphql"
}

on_get_user_agent <- function() {
  paste0("openneuro-r/", packageVersion("openneuro"))
}

on_get_cache_dir <- function() {
  dir <- Sys.getenv("OPENNEURO_CACHE_DIR",
    unset = tools::R_user_dir("openneuro", "cache"))
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

on_get_cache <- memoise::memoise(function(key) {
  key
})

on_parse_bids_filename <- function(filename) {
  parts <- list(
    subject = NA_character_,
    session = NA_character_,
    task = NA_character_,
    acquisition = NA_character_,
    run = NA_character_,
    processing = NA_character_,
    space = NA_character_,
    recording = NA_character_,
    suffix = NA_character_,
    extension = NA_character_
  )

  base <- basename(filename)
  ext <- tools::file_ext(base)
  parts$extension <- if (nzchar(ext)) paste0(".", ext) else ""

  no_ext <- base
  if (grepl("\\.nii\\.gz$", no_ext) || grepl("\\.nii\\.bz2$", no_ext) ||
      grepl("\\.nii\\.zst$", no_ext)) {
    no_ext <- sub("\\.[^.]+(\\.gz|\\.bz2|\\.zst)$", "", no_ext)
  } else {
    no_ext <- sub("\\.[^.]+$", "", no_ext)
  }

  segments <- strsplit(no_ext, "_")[[1]]

  suffix_candidates <- c()
  for (s in rev(segments)) {
    if (grepl("^[a-z]+[A-Z]", s) || grepl("\\.", s)) {
      suffix_candidates <- c(s, suffix_candidates)
    } else if (grepl("^[a-z]+$", s)) {
      suffix_candidates <- c(s, suffix_candidates)
    } else {
      break
    }
  }

  if (length(suffix_candidates) > 0) {
    parts$suffix <- paste(suffix_candidates, collapse = "_")
    segments <- segments[seq_len(length(segments) - length(suffix_candidates))]
  }

  for (seg in segments) {
    if (grepl("^sub-", seg)) {
      parts$subject <- sub("^sub-", "", seg)
    } else if (grepl("^ses-", seg)) {
      parts$session <- sub("^ses-", "", seg)
    } else if (grepl("^task-", seg)) {
      parts$task <- sub("^task-", "", seg)
    } else if (grepl("^acq-", seg)) {
      parts$acquisition <- sub("^acq-", "", seg)
    } else if (grepl("^run-", seg)) {
      parts$run <- sub("^run-", "", seg)
    } else if (grepl("^proc-", seg)) {
      parts$processing <- sub("^proc-", "", seg)
    } else if (grepl("^space-", seg)) {
      parts$space <- sub("^space-", "", seg)
    } else if (grepl("^rec-", seg)) {
      parts$recording <- sub("^rec-", "", seg)
    }
  }

  parts
}

on_build_file_url <- function(dataset_id, version, file_path) {
  paste0("https://openneuro.org/crn/datasets/", dataset_id,
         "/", version, "/", file_path)
}

on_check_dataset_id <- function(id) {
  if (!grepl("^ds\\d{6}$", id)) {
    cli::cli_abort("{.arg id} must start with {.val ds} followed by 6 digits, got {.val {id}}")
  }
}
