#' memberships datacube documentation
#'
#' @format The memberships datacube is a list that contains the
#' following 4 datasets: GPTAD_MEM, IEADB_MEM, TFDD_MEM, HUGGO_MEM.
#' For more information and references to each of the datasets used,
#' please use the `manydata::call_sources()` and `manydata::compare_dimensions()` functions.
#'\describe{
#' \item{GPTAD_MEM: }{A dataset with 2192 observations and the following
#' 9 variables: manyID, treatyID, Title, Begin, stateID, Signature, Force,
#' StateName, gptadID.}
#' \item{IEADB_MEM: }{A dataset with 15463 observations and the following
#' 12 variables: manyID, treatyID, Title, Begin, stateID, End, stateSignature,
#' Signature, Rat, Force, DocType, ieadbID.}
#' \item{TFDD_MEM: }{A dataset with 1832 observations and the following
#' 8 variables: manyID, treatyID, Title, Begin, stateID, Signature, tfddID, Memberships.}
#' \item{HUGGO_MEM: }{A dataset with 91664 observations and the following
#' 41 variables: manyID, treatyID, Title, Begin, stateID, StateName,
#' StateSignature, StateRatification, StateForce, StateEnd, Rat=Notif,
#' Accession, Succession, Signature, Force, End, gengID, url, ieaID, ecolexID,
#' StateForce2, StateForce3, StateEnd2, Term, Comments, Deposit, obsolete,
#' ProvisionalApp, Reservation, Notes, stateForce_ecolex, stateForce_iea,
#' Consent, Acceptance, Orig_noneng_title, match, gptadID, destaID, Title2,
#' Domain, Coder.}
#' }
#' @source
#' \itemize{
#' \item{GPTAD_MEM: }{
#' W. B. Group.
#' _Global Preferential Trade Agreement Database (GPTAD)_.
#' <https://wits.worldbank.org/gptad/library.aspx>. 2013.}
#' \item{IEADB_MEM: }{
#' R. B. Mitchell, L. B. Andonova, M. Axelrod, et al. “What we know (and could know) about internationalenvironmental agreements”.
#' _Global Environmental Politics_ 20.1 (2020), pp. 103-121.}
#' \item{TFDD_MEM: }{
#' O. College of Earth and O. S. U. Atmospheric Science.
#' _Product of the Transboundary Freshwater DisputeDatabase_. <http://transboundarywaters.science.oregonstate.edu.>. 2021.}
#' }
#' @section URL:
#' \itemize{
#' \item{GPTAD_MEM: }{
#' \url{https://wits.worldbank.org/gptad/library.aspx}
#' }
#' \item{IEADB_MEM: }{
#' \url{https://iea.uoregon.edu/country-members}
#' }
#' \item{TFDD_MEM: }{
#' \url{https://transboundarywaters.science.oregonstate.edu/}
#' }
#' \item{HUGGO_MEM: }{Hand-coded data by the GGO team.}
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
#' \item{HUGGO_MEM: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | NA | NA |
#' 
#' }
#' }
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(memberships, messydates::mreport)
#' ```
"memberships"
