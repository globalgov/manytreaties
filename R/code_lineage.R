#' Code lineage from agreement titles
#'
#' @param title A title column for agreements
#' @param datacube A datacube from the many packages ecosystem.
#' @return A list of lineages that combines agreement area
#' and agreement action.
#' @importFrom purrr map
#' @importFrom stringr str_squish
#' @importFrom stringi stri_trans_general
#' @examples
#' \dontrun{
#' code_lineage(title = sample(manyenviron::agreements$IEADB$Title, 30))
#' code_lineage(datacube = manyenviron::agreements)
#' }
#' @export
code_lineage <- function(title = NULL, datacube = NULL) {
  if (is.null(title) & is.null(datacube)) {
    stop("Please declare a title column or a many datacube")
  }
  # Get title variable from datacube, if available
  if (is.null(title)) {
    title <- unname(unlist(purrr::map(datacube, "Title")))
    vars <- unlist(purrr::map(datacube, names))
    if (any("Text" == vars)) { # Find text variable in datacube, if available
      txt <- unname(unlist(purrr::map(datacube, "Text")))
      txt <- read_clauses(standardise_treaty_text(txt), "preamble")
    }
  }
  # code entity and actions for titles
  title <- stringi::stri_trans_general(title, id = "Latin-ASCII")
  entity <- code_entity(title)
  domain <- code_domain(title)
  parties <- code_states(title)
  # Get entity and actions from preamble if missing from title
  if (exists("txt")) {
    entity <- ifelse(is.na(entity), code_entity(txt), entity)
    domain <- ifelse(is.na(domain), code_domain(txt), domain)
  }
  # Paste all together
  lineage <- ifelse(is.na(entity), paste0(parties, " - ", domain),
                    paste0(entity, " - ", domain))
  lineage <- gsub("- NA|NULL", "", lineage)
  lineage <- trimws(gsub("^-", "", lineage))
  lineage
}

#' Code Agreement Entity
#'
#' @param title Treaty titles
#' @return The region of the agreement
#' @importFrom stringr str_squish
#' @examples
#' \dontrun{
#' title <- sample(manyenviron::agreements$IEADB$Title, 30)
#' code_entity(title)
#' }
#' @export
code_entity <- function(title) {
  # Add a note about JavaScript
  usethis::ui_info("Please make sure JavaScript is installed.")
  # Download entity package
  pkgs <- NULL
  pkgs <- data.frame(utils::installed.packages())
  if (any(grepl("entity", pkgs$Package))) {
    remotes::install_github("trinker/entity")
    usethis::ui_info("Downloaded entity package.")
  }
  # Make sure necessary model is available (adapted from entity package)
  outcome <- "openNLPmodels.en" %in% list.files(.libPaths())
  if (!outcome) {
    utils::install.packages(
      "http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",
      repos = NULL,
      type = "source")
  }
  suppressWarnings(requireNamespace("entity", quietly = TRUE))
  # Code entity
  out <- suppressWarnings(entity::location_entity(title))
  # Code entity (using spacy for better results)
  # # Add a note about python
  # usethis::ui_info("Please make sure spacyr, minicinda, python, and spacy are installed.
  #                   This can be done by running 'spacyr::spacy_install()'")
  # spacyr::spacy_initialize()
  # out <- spacyr::entity_extract(spacyr::spacy_parse(title, entity = TRUE),
  #                       type = "named")
  #   dplyr::filter()
  #   dplyr::group_by(doc_id) %>%
  #   dplyr::summarise(entity_type = paste(entity_type, collapse = ", "),
  #                    entity = paste(gsub("_", " ", entity), collapse = ", "))
  # title <- data.frame(title)
  # title$doc_id <- paste0("text", as.numeric(rownames(title)))
  # out <- dplyr::left_join(title, out, by = "doc_id")
  # Remove states
  parties <- paste(countrynames$c, collapse = "|")
  out <- gsub(parties, "", out, ignore.case = TRUE)
  out <- gsub("^c|Britain|England", "", out)
  out <- gsub("[^[:alnum:]]", " ", out)
  out <- stringr::str_squish(out)
  out <- gsub("NULL", NA_character_, out)
  out <- ifelse(grepl("^$", out), NA_character_, out)
  out
}

