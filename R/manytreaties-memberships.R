#' memberships datacube documentation
#'
#' @format The memberships datacube is a list that contains the
#' following 3 datasets: GPTAD_MEM, IEADB_MEM, TFDD_MEM.
#' For more information and references to each of the datasets used,
#' please use the `manydata::call_sources()` and `manydata::compare_dimensions()` functions.
#'\describe{
#' \item{GPTAD_MEM: }{A dataset with 2192 observations and the following
#' 9 variables: manyID, treatyID, Title, Begin, stateID, Signature, Force, StateName, gptadID.}
#' \item{IEADB_MEM: }{A dataset with 15463 observations and the following
#' 12 variables: manyID, treatyID, Title, Begin, stateID, End, stateSignature, Signature, Rat, Force, DocType, ieadbID.}
#' \item{TFDD_MEM: }{A dataset with 1832 observations and the following
#' 8 variables: manyID, treatyID, Title, Begin, stateID, Signature, tfddID, Memberships.}
#' }
#' @source
#'\itemize{
#' \item{GPTAD_MEM: }{
#' [1] W. B. Group. _Global Preferential Trade Agreement Database (GPTAD)_.https://wits.worldbank.org/gptad/library.aspx. 2014.}
#' \item{IEADB_MEM: }{
#' [1] R. B. Mitchell, L. B. Andonova, M. Axelrod, et al. “What we know (and could know) aboutinternational environmental agreements”. In: _Global Environmental Politics_ 20.1 (2020), pp.103-121.}
#' \item{TFDD_MEM: }{
#' [1] O. College of Earth and O. S. U. Atmospheric Science. _Product of the Transboundary FreshwaterDispute Database_. <http://transboundarywaters.science.oregonstate.edu.>. 2021.}
#' }
#' @section URL:
#'\itemize{
#' \item{GPTAD_MEM: }{ \url https://wits.worldbank.org/gptad/library.aspx}
#' \item{IEADB_MEM: }{ \url https://iea.uoregon.edu/country-members}
#' \item{TFDD_MEM: }{ \url https://transboundarywaters.science.oregonstate.edu/}
#' }
#' @section Mapping:
#'\itemize{
#' \item{GPTAD_MEM: }{
#' Variable Mapping
#'
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | original_name | new_name |
#' Please fill in variable mapping here as above.}
#' \item{IEADB_MEM: }{
#' Variable Mapping
#'
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | original_name | new_name |
#' Please fill in variable mapping here as above.}
#' \item{TFDD_MEM: }{
#' Variable Mapping
#'
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | original_name | new_name |
#' Please fill in variable mapping here as above.}
#' }
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(memberships, messydates::mreport)
#' ```
"memberships"
