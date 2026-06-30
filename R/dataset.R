QUERY_DATASET <- 'query Dataset($id: ID!) {
  dataset(id: $id) {
    id
    name
    public
    metadata {
      datasetName
      seniorAuthor
      associatedPaperDOI
      species
      modalities
      studyDomain
      studyDesign
      studyLongitudinal
      grantFunderName
      grantIdentifier
      openneuroPaperDOI
      datasetUrl
      ages
      affirmedConsent
      affirmedDefaced
    }
    latestSnapshot {
      tag
      created
      size
      readme
      summary {
        subjects
        modalities
        tasks
        sessions
        totalFiles
        dataProcessed
      }
      issues {
        severity
        code
        reason
      }
    }
    brainInitiative
    publishDate
    public
  }
}'

#' Get metadata for an OpenNeuro dataset
#'
#' Retrieve detailed metadata for a single dataset, including description,
#' modalities, subjects, tasks, DOI, file count, and validation status.
#'
#' @param id Dataset accession number, e.g. `"ds000001"`.
#'
#' @return A named list of metadata fields. Use `str()` to explore.
#' @export
#'
#' @examples
#' \dontrun{
#' get_metadata("ds000001")
#' }
get_metadata <- function(id) {
  rlang::check_required(id)
  on_check_dataset_id(id)

  data <- on_graphql_request(QUERY_DATASET, list(id = id))

  ds <- data$dataset
  if (is.null(ds)) {
    cli::cli_abort("Dataset {.val {id}} not found.")
  }

  snap <- ds$latestSnapshot %||% list()
  summ <- snap$summary %||% list()
  meta <- ds$metadata %||% list()

  list(
    id = ds$id,
    name = ds$name %||% NA_character_,
    title = meta$datasetName %||% NA_character_,
    public = isTRUE(ds$public),
    doi = meta$associatedPaperDOI %||% NA_character_,
    openneuro_doi = meta$openneuroPaperDOI %||% NA_character_,
    senior_author = meta$seniorAuthor %||% NA_character_,
    species = meta$species %||% NA_character_,
    modalities = summ$modalities %||% character(),
    subjects = summ$subjects %||% character(),
    n_subjects = length(summ$subjects %||% character()),
    tasks = summ$tasks %||% character(),
    sessions = summ$sessions %||% character(),
    total_files = summ$totalFiles %||% NA_integer_,
    study_domain = meta$studyDomain %||% NA_character_,
    study_design = meta$studyDesign %||% NA_character_,
    study_longitudinal = meta$studyLongitudinal %||% NA_character_,
    grant_funder = meta$grantFunderName %||% NA_character_,
    grant_id = meta$grantIdentifier %||% NA_character_,
    ages = meta$ages %||% NA_character_,
    consent = isTRUE(meta$affirmedConsent),
    defaced = isTRUE(meta$affirmedDefaced),
    brain_initiative = isTRUE(ds$brainInitiative),
    publish_date = ds$publishDate %||% NA_character_,
    star_count = NA_integer_,
    version = snap$tag %||% NA_character_,
    version_created = snap$created %||% NA_character_,
    data_processed = isTRUE(summ$dataProcessed),
    readme = snap$readme %||% NA_character_
  )
}
