#' agreements datacube
#' @description The `agreements` datacube is a list containing 
#'   `r length(manytreaties::agreements)` datasets: 
#'   `r cli::pluralize("{names(manytreaties::agreements)}")`.
#'   It is a work-in-progress, so please let us know if you have any comments or suggestions.
#' @format 
#' \describe{
#' \item{HUGGO: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$HUGGO), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$HUGGO)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$HUGGO)}")`.}
#' \item{IEADB: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$IEADB), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$IEADB)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$IEADB)}")`.}
#' \item{HEIDI: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$HEIDI), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$HEIDI)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$HEIDI)}")`.}
#' \item{TREND: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$TREND), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$TREND)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$TREND)}")`.}
#' \item{LABPTA: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$LABPTA), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$LABPTA)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$LABPTA)}")`.}
#' \item{TOTA: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$TOTA), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$TOTA)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$TOTA)}")`.}
#' \item{GPTAD: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$GPTAD), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$GPTAD)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$GPTAD)}")`.}
#' \item{GHHR: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$GHHR), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$GHHR)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$GHHR)}")`.}
#' \item{WHO: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$WHO), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$WHO)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$WHO)}")`.}
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
#' \url{https://www.iea.ulaval.ca/en/agreements}
#' }
#' \item{HEIDI: }{
#' Morin, Jean-Frederic, and Chantal Blouin. 2019.
#' "How environmental treaties contribute to global health governance".
#' _Globalization and health_ 15.1, pp. 1-8.
#' \url{https://www.chaire-epi.ulaval.ca/en/data/heidi}
#' }
#' \item{TREND: }{
#' Morin, Jean-Frederic, Andreas Dür, and Lisa Lechner. 2018.
#' “Mapping the trade and environment nexus: Insights from a newdataset”.
#' _Global Environmental Politics_ 18.1, pp. 122-139.
#' \url{http://www.chaire-epi.ulaval.ca/en/trend}
#' }
#' \item{LABPTA: }{
#' Raess, Damien, Andreas Dür, and D. Sari. 2018.
#' “Protecting labor rights in preferential trade agreements: The role of trade unions, left governments, and skilled labor”.
#' _The Review of International Organizations_ 2.13, pp. 143-162.
#' \doi{10.1007/s11558-018-9301-z}
#' }
#' \item{TOTA: }{
#' Alschner, Wolfgang, Julia Seiermann, and Dmitriy Skougarevskiy. 2017.
#' “Text-as-data analysis of preferential trade agreements: Mapping the PTA landscape”.
#' UNCTAD Research Paper No. 5.
#' \url{https://github.com/mappingtreaties/tota.git}.
#' }
#' \item{GPTAD: }{
#' World Bank Group. 2014.
#' _Global Preferential Trade Agreement Database (GPTAD)_.
#' Online database. World Bank Group.
#' \url{https://wits.worldbank.org/gptad/library.aspx}
#' }
#' \item{GHHR: }{
#' B. M. Meier, O. A. Cabrera, A. Ayala, et al. 2012.
#' “Bridging international law and rights-based litigation: Mapping health-related rights through the development of the global health and human rights database”.
#' _Health & Human Rights._ 14, p. 20.
#' \url{https://www.globalhealthrights.org/instruments/instrument-region/}
#' }
#' \item{WHO: }{
#' World Health Organization. 2024.
#' _WHO MiNDbank: More Inclusiveness Needed in Disability and Development_. 
#' Online database. World Health Organization.
#' \url{https://extranet.who.int/mindbank/}
#' }
#' }
#' @section Mapping:
#' 
#' |  *manytreaties*  | *IEADB*  | *HEIDI* | *TREND* | *LABPTA* | *TOTA* | *GPTAD* | *GHHR* | *WHO* |
#' |:-----------------|:---------|:--------|:--------|:---------|:-------|:-------|:-------|:-------|
#' | Title  | Treaty Name | Name.of.the.agreement | Trade.Agreement | Name | name | Common.Name |
#' | Signature  | Signature Date | signature.date | Year | year | date_signed | Date.of.Signature |
#' | Force  | Date IEA entered into force | | Year | year | date_into_force | Date.of.Entry.into.Force | Year of adoption |
#' | mitchID  | IEA# | | | | |
#' | heidiID  | | ID | | | |
#' | labptaID  | | | | Number | |
#' | AgreementType  | Agreement Type (level 2) | | | | |
#' | DocType  | Inclusion | | | | | Type |
#' 
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(agreements, messydates::mreport)
#' ```
"agreements"
