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

