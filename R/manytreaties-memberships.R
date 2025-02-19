#' memberships datacube
#' @description The `memberships` datacube is a list containing 
#'   `r length(manytreaties::memberships)` datasets: 
#'   `r cli::pluralize("{names(manytreaties::memberships)}")`.
#'   It is a work-in-progress, so please let us know if you have any comments or suggestions.
#' @format 
#' \describe{
#' \item{HUGGO: }{A dataset with `r prettyNum(nrow(manytreaties::memberships$HUGGO), big.mark=",")` 
#' observations and `r ncol(manytreaties::memberships$HUGGO)` variables: 
#' `r cli::pluralize("{names(manytreaties::memberships$HUGGO)}")`.}
#' \item{HUGGO: }{A dataset with `r prettyNum(nrow(manytreaties::memberships$IEADB), big.mark=",")` 
#' observations and `r ncol(manytreaties::memberships$IEADB)` variables: 
#' `r cli::pluralize("{names(manytreaties::memberships$IEADB)}")`.}
#' \item{HUGGO: }{A dataset with `r prettyNum(nrow(manytreaties::memberships$GPTAD), big.mark=",")` 
#' observations and `r ncol(manytreaties::memberships$GPTAD)` variables: 
#' `r cli::pluralize("{names(manytreaties::memberships$GPTAD)}")`.}
#' \item{HUGGO: }{A dataset with `r prettyNum(nrow(manytreaties::memberships$TFDD), big.mark=",")` 
#' observations and `r ncol(manytreaties::memberships$TFDD)` variables: 
#' `r cli::pluralize("{names(manytreaties::memberships$TFDD)}")`.}
#' }
#' For more information and references to each of the datasets used,
#' please use the `manydata::call_sources()` and `manydata::compare_dimensions()` functions.
#' @source
#' \itemize{
#' \item{GPTAD: }{
#' W. B. Group.
#' _Global Preferential Trade Agreement Database (GPTAD)_.
#' <https://wits.worldbank.org/gptad/library.aspx>. 2013.}
#' \item{IEADB: }{
#' R. B. Mitchell, L. B. Andonova, M. Axelrod, et al. “What we know (and could know) about international environmental agreements”.
#' _Global Environmental Politics_ 20.1 (2020), pp. 103-121. \url{https://iea.uoregon.edu/country-members}}
#' \item{TFDD: }{
#' O. College of Earth and O. S. U. Atmospheric Science.
#' _Product of the Transboundary Freshwater DisputeDatabase_.
#' <http://transboundarywaters.science.oregonstate.edu.>. 2021.}
#' }
#' @section Mapping:
#' \itemize{
#' \item{GPTAD_MEM: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | Membership | StateName |
#' | Common.Name | Title |
#' | Date.of.Signature | Signature |
#' | Date.of.Entry.into.Force | Force|
#' 
#' }
#' \item{IEADB_MEM: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | country | stateID |
#' | treatyname | Title |
#' | tsig | Signature |
#' | csig | stateSignature |
#' | crat | Rat |
#' | tterm | End |
#' | ceif3 | Force |
#' | ceif4 | Force2 |
#' | mitch_id | ieadbID |
#' | inclusion | DocType |
#' 
#' }
#' \item{TFDD_MEM: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | DateSigned | Signature |
#' | '2016Update ID' | tfddID |
#' | CCODE | stateID |
#' | DocumentName | Title |
#' | Signatories | Memberships |
#' 
#' }
#' }
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(memberships, messydates::mreport)
#' ```
"memberships"
