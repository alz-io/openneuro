#' Parse BIDS entity information from file paths
#'
#' Given a file listing from [list_files()], extract BIDS entities (subject,
#' session, task, acquisition, run, processing, space, suffix, extension)
#' from each filename.
#'
#' @param files A data frame returned by [list_files()], or a character vector
#'   of file paths/filenames.
#'
#' @return A [tibble][tibble::tibble-package] with one row per file and
#'   columns: `filename`, `subject`, `session`, `task`, `acquisition`, `run`,
#'   `processing`, `space`, `recording`, `suffix`, `extension`.
#' @export
#'
#' @examples
#' \dontrun{
#' fls <- list_files("ds000001", recursive = TRUE)
#' fls_idx <- bids_index(fls)
#' dplyr::filter(fls_idx, suffix == "bold.nii.gz")
#' }
bids_index <- function(files) {
  if (is.data.frame(files)) {
    filenames <- files$filename
  } else if (is.character(files)) {
    filenames <- files
    files <- NULL
  } else {
    cli::cli_abort("{.arg files} must be a data frame or character vector.")
  }

  if (length(filenames) == 0) {
    return(tibble::tibble(
      filename = character(), subject = character(),
      session = character(), task = character(),
      acquisition = character(), run = character(),
      processing = character(), space = character(),
      recording = character(), suffix = character(),
      extension = character()
    ))
  }

  rows <- lapply(seq_along(filenames), function(i) {
    parts <- on_parse_bids_filename(filenames[i])
    row <- data.frame(
      filename = filenames[i],
      subject = parts$subject,
      session = parts$session,
      task = parts$task,
      acquisition = parts$acquisition,
      run = if (is.na(parts$run)) NA_integer_ else as.integer(parts$run),
      processing = parts$processing,
      space = parts$space,
      recording = parts$recording,
      suffix = parts$suffix,
      extension = parts$extension,
      stringsAsFactors = FALSE
    )

    if (!is.null(files) && ncol(files) > 1) {
      row <- cbind(row, files[i, setdiff(names(files), "filename"), drop = FALSE])
    }

    row
  })

  tbl <- tibble::as_tibble(do.call(rbind, rows))
  tbl$run <- as.integer(tbl$run)
  tbl
}
