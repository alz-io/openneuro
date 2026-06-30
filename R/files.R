QUERY_SNAPSHOT_FILES <- 'query SnapshotFiles(
  $datasetId: ID!, $tag: String!, $tree: String, $recursive: Boolean
) {
  snapshot(datasetId: $datasetId, tag: $tag) {
    files(tree: $tree, recursive: $recursive) {
      id
      filename
      size
      directory
      annexed
      urls
    }
  }
}'

#' List files in an OpenNeuro dataset
#'
#' Returns a tibble of all files in a dataset. By default lists the top-level
#' directory. Use `recursive = TRUE` to get the full file tree.
#'
#' @param id Dataset accession number, e.g. `"ds000001"`.
#' @param version Snapshot version tag. Defaults to the latest.
#' @param recursive If `TRUE`, return the complete recursive file listing.
#' @param tree Git tree ID for navigating subdirectories. Typically not needed
#'   when `recursive = TRUE`.
#'
#' @return A [tibble][tibble::tibble-package] with columns: `id`, `filename`,
#'   `size`, `directory`, `annexed`, `url`.
#' @export
#'
#' @examples
#' \dontrun{
#' list_files("ds000001")
#' list_files("ds000001", recursive = TRUE)
#' }
list_files <- function(id, version = NULL, recursive = TRUE, tree = NULL) {
  rlang::check_required(id)
  on_check_dataset_id(id)

  if (is.null(version)) {
    meta <- get_metadata(id)
    version <- meta$version
    if (is.null(version) || is.na(version)) {
      cli::cli_abort("Could not determine latest snapshot version for {.val {id}}.")
    }
  }

  vars <- list(datasetId = id, tag = version,
               tree = tree, recursive = recursive)

  if (is.null(tree)) vars$tree <- NULL

  data <- on_graphql_request(QUERY_SNAPSHOT_FILES, vars)

  files <- data$snapshot$files
  if (is.null(files)) {
    return(tibble::tibble(
      id = character(), filename = character(), size = integer(),
      directory = logical(), annexed = logical(), url = character()
    ))
  }

  rows <- lapply(files, function(f) {
    url <- if (length(f$urls) > 0) f$urls[[1]] else NA_character_
    list(
      id = f$id %||% NA_character_,
      filename = f$filename %||% NA_character_,
      size = as.numeric(f$size %||% 0),
      directory = isTRUE(f$directory),
      annexed = isTRUE(f$annexed),
      url = url
    )
  })

  tbl <- tibble::as_tibble(do.call(rbind, lapply(rows, function(r) {
    data.frame(
      id = r$id, filename = r$filename, size = r$size,
      directory = r$directory, annexed = r$annexed,
      url = r$url, stringsAsFactors = FALSE
    )
  })))

  tbl$size <- as.numeric(tbl$size)
  tbl
}

#' Download files from an OpenNeuro dataset
#'
#' Download specific files from a dataset using their URLs as returned by
#' [list_files()]. Supports parallel downloads with progress bars.
#'
#' @param files A data frame with columns `filename` and `url`, as returned
#'   by [list_files()].
#' @param dest_dir Destination directory. Files are placed in a subdirectory
#'   named after the dataset ID.
#' @param dataset_id Optional dataset ID used to construct the dest path.
#' @param version Optional version tag for path construction.
#' @param overwrite Overwrite existing files. Default `FALSE`.
#' @param quiet Suppress progress output. Default `FALSE`.
#' @param n_parallel Number of simultaneous downloads. Default 1.
#'
#' @return A character vector of downloaded file paths, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' fls <- list_files("ds000001", recursive = TRUE)
#' t1w <- fls[grep("T1w", fls$filename), ]
#' download_files(t1w, dest_dir = "data")
#' }
download_files <- function(files, dest_dir = "openneuro_data",
                           dataset_id = NULL, version = NULL,
                           overwrite = FALSE, quiet = FALSE,
                           n_parallel = 1L) {
  rlang::check_required(files)

  if (nrow(files) == 0) {
    cli::cli_inform("No files to download.")
    return(invisible(character()))
  }

  if (!is.null(dataset_id)) {
    dest <- fs::path(dest_dir, dataset_id)
    if (!is.null(version)) dest <- fs::path(dest, version)
  } else {
    dest <- dest_dir
  }

  fs::dir_create(dest)

  file_df <- files[!files$directory, , drop = FALSE]
  if (nrow(file_df) == 0) {
    cli::cli_inform("No files to download (all entries are directories).")
    return(invisible(character()))
  }

  dest_paths <- fs::path(dest, file_df$filename)
  existing <- file.exists(dest_paths)

  if (!overwrite && any(existing)) {
    cli::cli_inform(c(
      "!" = "{sum(existing)} file{?s} already exist{?s}, skipping.",
      "i" = "Use {.code overwrite = TRUE} to re-download."
    ))
  }

  to_dl <- if (overwrite) seq_len(nrow(file_df)) else which(!existing)

  if (length(to_dl) == 0) {
    cli::cli_inform("All files already downloaded.")
    return(invisible(dest_paths))
  }

  urls <- file_df$url[to_dl]
  paths <- dest_paths[to_dl]

  for (i in seq_along(paths)) {
    fs::dir_create(dirname(paths[i]))
  }

  if (!quiet) {
    pb <- progress::progress_bar$new(
      format = "  downloading [:bar] :percent ETA: :eta",
      total = length(to_dl), clear = FALSE, width = 60
    )
  }

  for (i in seq_along(urls)) {
    if (!quiet) pb$tick()
    curl::curl_download(urls[i], paths[i], quiet = quiet,
                        mode = "wb")
  }

  invisible(dest_paths)
}

#' Download an entire OpenNeuro dataset
#'
#' Download all files from a dataset using the recursive file listing.
#' Downloads directly from OpenNeuro S3 storage.
#'
#' @param id Dataset accession number, e.g. `"ds000001"`.
#' @param version Snapshot version tag. Defaults to the latest.
#' @param subjects Optional character vector of subject IDs to download
#'   (e.g. `c("01", "02")`).
#' @param dest_dir Destination directory.
#' @param overwrite Overwrite existing files.
#' @param quiet Suppress progress output.
#'
#' @return Character vector of downloaded file paths, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' download_dataset("ds000001", subjects = c("01", "02"))
#' }
download_dataset <- function(id, version = NULL, subjects = NULL,
                             dest_dir = "openneuro_data",
                             overwrite = FALSE, quiet = FALSE) {
  rlang::check_required(id)
  on_check_dataset_id(id)

  cli::cli_inform("Retrieving file listing for {.val {id}}...")
  files <- list_files(id, version = version, recursive = TRUE)

  if (!is.null(subjects)) {
    patterns <- paste0("^sub-", subjects, "/")
    keep <- vapply(seq_len(nrow(files)), function(i) {
      any(vapply(patterns, function(p) grepl(p, files$filename[i]),
                 logical(1)))
    }, logical(1))
    files <- files[keep, , drop = FALSE]
    cli::cli_inform("Filtered to {sum(!files$directory)} file{?s} for {length(subjects)} subject{?s}.")
  }

  download_files(files, dest_dir = dest_dir, dataset_id = id,
                 version = version, overwrite = overwrite,
                 quiet = quiet)
}
