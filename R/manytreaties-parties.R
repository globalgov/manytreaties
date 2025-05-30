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
#' \item{DESTA: }{A dataset with `r prettyNum(nrow(manytreaties::parties$DESTA), big.mark=",")` 
#' observations and `r ncol(manytreaties::parties$DESTA)` variables: 
#' `r cli::pluralize("{names(manytreaties::parties$DESTA)}")`.}
#' \item{GPTAD: }{A dataset with `r prettyNum(nrow(manytreaties::parties$GPTAD), big.mark=",")` 
#' observations and `r ncol(manytreaties::parties$GPTAD)` variables: 
#' `r cli::pluralize("{names(manytreaties::parties$GPTAD)}")`.}
#' \item{TFDD: }{A dataset with `r prettyNum(nrow(manytreaties::parties$TFDD), big.mark=",")` 
#' observations and `r ncol(manytreaties::parties$TFDD)` variables: 
#' `r cli::pluralize("{names(manytreaties::parties$TFDD)}")`.}
#' }
#' For more information and references to each of the datasets used,
#' please use the `manydata::call_sources()` and `manydata::compare_dimensions()` functions.
#' @source
#'   `r call_citations(parties, output = "help")`
#' @section Mapping:
#' 
#' |  *manytreaties*  | *IEADB*  | *DESTA* | *GPTAD* | *TFDD* |
#' |:-----------------|:---------|:--------|:--------|:-------|
#' | stateID  | country | Member | | CCODE |
#' | Signature  | csig | year |  Date.of.Signature | DateSigned |
#' | Rat  | crat | | | |
#' | Force  | ceif3 | entryforceyear | Date.of.Entry.into.Force | |
#' | Force2  | ceif4 | | | |
#' | End  | tterm | | | |
#' | Title  | treatyname | name | Common.Name | DocumentName |
#' | Memberships  | | | StateName | Signatories |
#' | destaID | | base_treaty | | |
#' | tfddID  | | | | 2016Update ID |
#' | mitchID  | ieadbID | | | |
#' | DocType  | inclusion | | | |
#' 
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(parties, mreport)
#' ```
"parties"

info_parties <- tibble::tibble(Dataset = names(utils::data(parties, package = "manytreaties")),
                                Source = c("Hollway, James, Henrique Sposito, and Jael Tan. 2021. International agreements for manydata.",
                                           "Mitchell, Ron B. et al. 2020. 'What we know (and could know) about international environmental agreements'. _Global Environmental Politics_ 20.1, pp. 103-121.",
                                           "Duer, Andreas, Leonardo Baccini, and Manfred Elsig. 2014. 'The Design of International Trade Agreements: Introducing a New Database'. _The Review of International Organizations_ 9.3, pp. 353-375.",
                                           "World Bank Group. 2014. _Global Preferential Trade Agreement Database (GPTAD)_. Online database. World Bank Group.",
                                           "Oregon College of Earth and Oregon State University Atmospheric Science. 2021. _Product of the Transboundary Freshwater Dispute Database_."),
                                URL = c("",
                                        "https://www.iea.ulaval.ca/en/country-members",
                                        "https://www.designoftradeagreements.org/downloads/",
                                        "https://wits.worldbank.org/gptad/library.aspx",
                                        "http://transboundarywaters.science.oregonstate.edu"))

