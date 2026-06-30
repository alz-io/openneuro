on_graphql_request <- function(query, variables = list()) {
  req <- httr2::request(on_get_url()) |>
    httr2::req_body_json(list(
      query = query,
      variables = variables
    )) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_user_agent(on_get_user_agent())

  resp <- httr2::req_perform(req)
  parsed <- httr2::resp_body_json(resp)

  if (!is.null(parsed$errors)) {
    msgs <- vapply(parsed$errors, `[[`, character(1), "message")
    if (is.null(parsed$data)) {
      cli::cli_abort(c(
        "OpenNeuro API request failed",
        "x" = msgs[1]
      ))
    }
    cli::cli_warn(c(
      "Partial errors from OpenNeuro API",
      "!" = msgs[1],
      "i" = "Results may be incomplete."
    ))
  }

  parsed$data
}

on_graphql_paginated <- function(query_template, var_template,
                                 variables = list(), page_size = 50L) {
  all_edges <- list()
  cursor <- NULL

  repeat {
    vars <- utils::modifyList(variables, var_template(page_size, cursor))
    data <- on_graphql_request(query_template, vars)

    conn <- data[[1]]
    edges <- conn$edges
    all_edges <- c(all_edges, edges)

    page_info <- conn$pageInfo
    if (is.null(page_info) || is.null(page_info$hasNextPage) ||
        !isTRUE(page_info$hasNextPage)) {
      break
    }
    cursor <- page_info$endCursor %||% edges[[length(edges)]]$cursor
  }

  all_edges
}

`%||%` <- function(x, y) if (is.null(x)) y else x
