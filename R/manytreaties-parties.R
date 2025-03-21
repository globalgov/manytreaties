#' parties datacube
#' @description `r describe_datacube(manytreaties::parties)`.
#'   It is a work-in-progress, so please let us know if you have any comments or suggestions.
#' @format 
#' \describe{
#' \item{HUGGO <img src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by.png" height="12"/>:}{A dataset with `r prettyNum(nrow(manytreaties::parties$HUGGO), big.mark=",")` 
#' observations and `r ncol(manytreaties::parties$HUGGO)` variables: 
#' `r cli::pluralize("{names(manytreaties::parties$HUGGO)}")`.}
#' \item{IEADB: }{A dataset with `r prettyNum(nrow(manytreaties::parties$IEADB), big.mark=",")` 
#' observations and `r ncol(manytreaties::parties$IEADB)` variables: 
#' `r cli::pluralize("{names(manytreaties::parties$IEADB)}")`.}
#' \item{GPTAD <img src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by.png" height="12"/>:}{A dataset with `r prettyNum(nrow(manytreaties::parties$GPTAD), big.mark=",")` 
#' observations and `r ncol(manytreaties::parties$GPTAD)` variables: 
#' `r cli::pluralize("{names(manytreaties::parties$GPTAD)}")`.}
#' \item{TFDD <img src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/cc-zero.png" height="12"/>:}{A dataset with `r prettyNum(nrow(manytreaties::parties$TFDD), big.mark=",")` 
#' observations and `r ncol(manytreaties::parties$TFDD)` variables: 
#' `r cli::pluralize("{names(manytreaties::parties$TFDD)}")`.}
#' }
#' For more information and references to each of the datasets used,
#' please use the `manydata::call_sources()` and `manydata::compare_dimensions()` functions.
#' @source
#'   `r call_citations(parties, output = "help")`
#' @section Mapping:
#' 
#' |  *manytreaties*  | *IEADB*  | *GPTAD* | *TFDD* |
#' |:-----------------|:---------|:--------|:--------|
#' | stateID  | country | | CCODE |
#' | Signature  | csig | Date.of.Signature | DateSigned |
#' | Rat  | crat | | |
#' | Force  | ceif3 | Date.of.Entry.into.Force | |
#' | Force2  | ceif4 | | |
#' | End  | tterm | | |
#' | Title  | treatyname | Common.Name | DocumentName |
#' | Memberships  | | StateName | Signatories |
#' | tfddID  | | | 2016Update ID |
#' | mitchID  | ieadbID | | |
#' | AgreementType  | | | |
#' | DocType  | inclusion | | |
#' 
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(parties, mreport)
#' ```
"parties"

info_parties <- tibble::tibble(Dataset = names(data(parties, package = "manytreaties")),
                                Source = c("Hollway, James, Henrique Sposito, and Jael Tan. 2021. International agreements for manydata.",
                                           "Mitchell, Ron B. et al. 2020. 'What we know (and could know) about international environmental agreements'. _Global Environmental Politics_ 20.1, pp. 103-121.",
                                           "World Bank Group. 2014. _Global Preferential Trade Agreement Database (GPTAD)_. Online database. World Bank Group.",
                                           "Oregon College of Earth and Oregon State University Atmospheric Science. 2021. _Product of the Transboundary Freshwater Dispute Database_."),
                                URL = c("",
                                        "https://www.iea.ulaval.ca/en/country-members",
                                        "https://wits.worldbank.org/gptad/library.aspx",
                                        "http://transboundarywaters.science.oregonstate.edu"))

