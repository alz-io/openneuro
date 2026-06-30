QUERY_SEARCH <- 'query SearchDatasets(
  $first: Int, $after: String,
  $modality: String, $species: String, $authors: [String!],
  $diagnosis: String, $bodyParts: [String!], $tasks: [String!],
  $studyDomains: [String!], $sex: String,
  $subjectCountRange: [Int], $ageRange: [Int],
  $scannerManufacturers: [String!],
  $scannerManufacturersModelNames: [String!],
  $bidsDatasetType: String, $brainInitiative: Boolean,
  $studyStructure: String, $tracerNames: [String!],
  $tracerRadionuclides: [String!], $dateRange: String,
  $keywords: [String!], $sortBy: SearchSortOption
) {
  advancedSearch(first: $first, after: $after, query: {
    modality: $modality
    species: $species
    authors: $authors
    diagnosis: $diagnosis
    bodyParts: $bodyParts
    tasks: $tasks
    studyDomains: $studyDomains
    sex: $sex
    subjectCountRange: $subjectCountRange
    ageRange: $ageRange
    scannerManufacturers: $scannerManufacturers
    scannerManufacturersModelNames: $scannerManufacturersModelNames
    bidsDatasetType: $bidsDatasetType
    brainInitiative: $brainInitiative
    studyStructure: $studyStructure
    tracerNames: $tracerNames
    tracerRadionuclides: $tracerRadionuclides
    dateRange: $dateRange
    keywords: $keywords
    sortBy: $sortBy
  }) {
    edges {
      node {
        id
        name
        public
        latestSnapshot {
          tag
          summary {
            subjects
            modalities
            tasks
            sessions
            totalFiles
          }
        }
        metadata {
          datasetName
          seniorAuthor
          associatedPaperDOI
          species
          modalities
        }
      }
    }
  }
}'

QUERY_DATASETS <- 'query Datasets(
  $first: Int, $after: String,
  $modality: String, $orderBy: DatasetSort
) {
  datasets(first: $first, after: $after,
    modality: $modality, orderBy: $orderBy
  ) {
    edges {
      node {
        id
        name
        public
        latestSnapshot {
          tag
          summary {
            subjects
            modalities
            tasks
            sessions
            totalFiles
          }
        }
        metadata {
          datasetName
          seniorAuthor
          associatedPaperDOI
          species
          modalities
        }
      }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}'

#' Search OpenNeuro datasets
#'
#' Search for datasets on OpenNeuro using the advanced search API.
#' Supports filtering by modality, species, task, and other BIDS fields.
#'
#' @param modality Filter by imaging modality (e.g. `"mri"`, `"pet"`, `"eeg"`).
#' @param species Filter by species (e.g. `"Human"`, `"Mouse"`).
#' @param task Filter by task name (e.g. `"stroop"`, `"rest"`).
#' @param diagnosis Filter by diagnosis.
#' @param sex Filter by sex (`"male"`, `"female"`, `"both"`).
#' @param min_subjects Minimum number of subjects.
#' @param max_subjects Maximum number of subjects.
#' @param bids_dataset_type BIDS dataset type (`"raw"` or `"derivative"`).
#' @param brain_initiative Filter for Brain Initiative datasets.
#' @param study_domain Filter by study domain.
#' @param body_part Filter by body part.
#' @param author Filter by author name.
#' @param keywords Free-text keywords.
#' @param ... Additional search parameters passed to the API.
#' @param n Maximum number of datasets to return.
#' @param order_by Sort order. One of `"name"`, `"created"`, `"downloads"`,
#'   `"stars"`, `"views"`.
#'
#' @return A [tibble][tibble::tibble-package] with one row per dataset.
#' @export
#'
#' @examples
#' \dontrun{
#' search_datasets(modality = "mri", n = 5)
#' search_datasets(task = "stroop", min_subjects = 50)
#' }
search_datasets <- function(modality = NULL,
                            species = NULL,
                            task = NULL,
                            diagnosis = NULL,
                            sex = NULL,
                            min_subjects = NULL,
                            max_subjects = NULL,
                            bids_dataset_type = NULL,
                            brain_initiative = NULL,
                            study_domain = NULL,
                            body_part = NULL,
                            author = NULL,
                            keywords = NULL,
                            ...,
                            n = 20,
                            order_by = NULL) {

  dots <- rlang::list2(...)

  if (!is.null(order_by)) {
    order_by <- match.arg(order_by, c("name", "created", "downloads",
                                       "stars", "views", "publishDate"))
  }

  as_list_field <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.character(x) && length(x) == 1) list(x) else as.list(x)
  }

  vars <- list(
    first = as.integer(n),
    after = NULL,
    modality = modality %||% dots$modality,
    species = species %||% dots$species,
    tasks = as_list_field(task %||% dots$task),
    diagnosis = diagnosis %||% dots$diagnosis,
    sex = sex %||% dots$sex,
    subjectCountRange = as_list_field(min_subjects %||% dots$min_subjects),
    bidsDatasetType = bids_dataset_type %||% dots$bids_dataset_type,
    brainInitiative = brain_initiative %||% dots$brain_initiative,
    studyDomains = as_list_field(study_domain %||% dots$study_domain),
    bodyParts = as_list_field(body_part %||% dots$body_part),
    authors = as_list_field(author %||% dots$author),
    keywords = as_list_field(keywords %||% dots$keywords),
    sortBy = order_by %||% dots$sort_by,
    ageRange = dots$age_range,
    scannerManufacturers = as_list_field(dots$scanner_manufacturer),
    scannerManufacturersModelNames = as_list_field(dots$scanner_model),
    studyStructure = dots$study_structure,
    tracerNames = as_list_field(dots$tracer_name),
    tracerRadionuclides = as_list_field(dots$tracer_radionuclide),
    dateRange = dots$date_range
  )

  vars <- Filter(Negate(is.null), vars)

  
  data <- on_graphql_request(QUERY_SEARCH, vars)

  if (is.null(data$advancedSearch) || is.null(data$advancedSearch$edges)) {
    return(tibble::tibble(
      id = character(), name = character(), public = logical(),
      subjects = integer(), modalities = list(), tasks = list(),
      dataset_name = character(), senior_author = character(),
      doi = character(), total_files = integer()
    ))
  }

  edges <- data$advancedSearch$edges
  edges <- Filter(function(e) {
    !is.null(e$node) && !is.null(e$node$id)
  }, edges)
  parse_search_results(edges)
}

parse_search_results <- function(edges) {
  if (length(edges) == 0) {
    return(tibble::tibble(
      id = character(), name = character(), public = logical(),
      version = character(), subjects = integer(),
      modalities = list(), tasks = list(),
      dataset_name = character(), senior_author = character(),
      doi = character(), species = character(), total_files = integer()
    ))
  }

  ids <- character(length(edges))
  names <- character(length(edges))
  publics <- logical(length(edges))
  versions <- character(length(edges))
  subjects <- integer(length(edges))
  modalities <- vector("list", length(edges))
  tasks <- vector("list", length(edges))
  dataset_names <- character(length(edges))
  senior_authors <- character(length(edges))
  dois <- character(length(edges))
  species <- character(length(edges))
  total_filess <- integer(length(edges))

  for (i in seq_along(edges)) {
    n <- edges[[i]]$node
    snap <- n$latestSnapshot %||% list()
    summ <- snap$summary %||% list()
    meta <- n$metadata %||% list()

    ids[i] <- n$id %||% NA_character_
    names[i] <- n$name %||% NA_character_
    publics[i] <- isTRUE(n$public)
    versions[i] <- snap$tag %||% NA_character_
    subjects[i] <- length(summ$subjects %||% character())
    modalities[[i]] <- summ$modalities %||% character()
    tasks[[i]] <- summ$tasks %||% character()
    dataset_names[i] <- meta$datasetName %||% NA_character_
    senior_authors[i] <- meta$seniorAuthor %||% NA_character_
    dois[i] <- meta$associatedPaperDOI %||% NA_character_
    species[i] <- meta$species %||% NA_character_
    total_filess[i] <- summ$totalFiles %||% NA_integer_
  }

  tibble::tibble(
    id = ids, name = names, public = publics,
    version = versions, subjects = subjects,
    modalities = modalities, tasks = tasks,
    dataset_name = dataset_names, senior_author = senior_authors,
    doi = dois, species = species, total_files = total_filess
  )
}
