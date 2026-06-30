#' Manage the OpenNeuro local cache
#'
#' Configure and inspect the local data cache. OpenNeuro downloads are cached
#' under the directory returned by `tools::R_user_dir("openneuro", "cache")`
#' unless overridden by the `OPENNEURO_CACHE_DIR` environment variable or the
#' `path` argument.
#'
#' @param path Character string, path to the cache directory. If `NULL` (the
#'   default), uses the current cache directory.
#' @param clear If `TRUE`, removes all cached files and returns the cache path.
#' @param info If `TRUE`, prints cache summary and returns invisibly.
#'
#' @return The cache directory path (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' openneuro_cache()
#' openneuro_cache(info = TRUE)
#' openneuro_cache(path = "~/my_cache", clear = TRUE)
#' }
openneuro_cache <- function(path = NULL, clear = FALSE, info = FALSE) {
  if (!is.null(path)) {
    cache_path <- path
  } else {
    cache_path <- on_get_cache_dir()
  }

  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
  }

  if (clear) {
    unlink(cache_path, recursive = TRUE)
    dir.create(cache_path, showWarnings = FALSE)
    cli::cli_inform("Cache cleared at {.path {cache_path}}")
    return(invisible(cache_path))
  }

  if (info) {
    files <- list.files(cache_path, recursive = TRUE)
    total_size <- sum(file.size(file.path(cache_path, files)), na.rm = TRUE)
    cli::cli_inform(c(
      "OpenNeuro cache directory: {.path {cache_path}}",
      ">" = "{length(files)} cached file{?s}",
      ">" = "Total size: {format(structure(total_size, class = 'object_size'), units = 'auto')}"
    ))
    return(invisible(cache_path))
  }

  invisible(cache_path)
}
