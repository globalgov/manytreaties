#' parties datacube
#' @description The `parties` datacube is a list containing 
#'   `r length(manytreaties::parties)` datasets: 
#'   `r cli::pluralize("{names(manytreaties::parties)}")`.
#'   It is a work-in-progress, so please let us know if you have any comments or suggestions.
#' @format 
#' \describe{
#' \item{HUGGO: }{A dataset with `r prettyNum(nrow(manytreaties::parties$HUGGO), big.mark=",")` 
#' observations and `r ncol(manytreaties::parties$HUGGO)` variables: 
#' `r cli::pluralize("{names(manytreaties::parties$HUGGO)}")`.}
#' \item{IEADB: }{A dataset with `r prettyNum(nrow(manytreaties::parties$IEADB), big.mark=",")` 
#' observations and `r ncol(manytreaties::parties$IEADB)` variables: 
#' `r cli::pluralize("{names(manytreaties::parties$IEADB)}")`.}
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
#' \itemize{
#' \item{HUGGO: }{
#' Hollway, James, Henrique Sposito, and Jael Tan. 2021.
#' International agreements for manydata.
#' }
#' \item{IEADB: }{
#' Mitchell, Ron B. et al. 2020.
#' "What we know (and could know) about international environmental agreements".
#' _Global Environmental Politics_ 20.1, pp. 103-121.
#' \url{https://www.iea.ulaval.ca/en/country-members}
#' }
#' \item{GPTAD: }{
#' World Bank Group. 2014.
#' _Global Preferential Trade Agreement Database (GPTAD)_.
#' Online database. World Bank Group.
#' \url{https://wits.worldbank.org/gptad/library.aspx}
#' }
#' \item{TFDD: }{
#' Oregon College of Earth and Oregon State University Atmospheric Science. 2021.
#' _Product of the Transboundary Freshwater Dispute Database_.
#' \url{http://transboundarywaters.science.oregonstate.edu}.
#' }
#' }
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
#' lapply(parties, messydates::mreport)
#' ```
"parties"
