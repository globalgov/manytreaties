#' agreements datacube documentation
#' @description `r describe_datacube(manytreaties::agreements)`.
#'   It is a work-in-progress, so please let us know if you have any comments or suggestions.
#' @format 
#' \describe{
#' \item{HUGGO <img src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by.png" height="12"/>:}{A dataset with `r prettyNum(nrow(manytreaties::agreements$HUGGO), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$HUGGO)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$HUGGO)}")`.}
#' \item{IEADB: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$IEADB), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$IEADB)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$IEADB)}")`.}
#' \item{HEIDI: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$HEIDI), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$HEIDI)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$HEIDI)}")`.}
#' \item{TFDD: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$TFDD), big.mark=",")`
#'   observations and `r ncol(manytreaties::agreements$TFDD)` variables:
#'   `r cli::pluralize("{names(manytreaties::agreements$TFDD)}")`.}
#' \item{TREND: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$TREND), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$TREND)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$TREND)}")`.}
#' \item{LABPTA <img src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by.png" height="12"/>:}{A dataset with `r prettyNum(nrow(manytreaties::agreements$LABPTA), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$LABPTA)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$LABPTA)}")`.}
#' \item{TOTA <img src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by.png" height="12"/>:}{A dataset with `r prettyNum(nrow(manytreaties::agreements$TOTA), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$TOTA)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$TOTA)}")`.}
#' \item{GPTAD <img src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by.png" height="12"/>:}{A dataset with `r prettyNum(nrow(manytreaties::agreements$GPTAD), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$GPTAD)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$GPTAD)}")`.}
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
#' \item{TFDD: }{
#' College of Earth, Ocean and Atmospheric Science, Oregon State University. 2021.
#' Product of the Transboundary FreshwaterDispute Database.
#' \url{http://transboundarywaters.science.oregonstate.edu}
#' }
#' \item{TREND: }{
#' Morin, Jean-Frederic, Andreas Dür, and Lisa Lechner. 2018.
#' "Mapping the trade and environment nexus: Insights from a new dataset".
#' _Global Environmental Politics_ 18.1, pp. 122-139.
#' \url{http://www.chaire-epi.ulaval.ca/en/trend}
#' }
#' \item{LABPTA: }{
#' Raess, Damien, Andreas Dür, and D. Sari. 2018.
#' "Protecting labor rights in preferential trade agreements: The role of trade unions, left governments, and skilled labor".
#' _The Review of International Organizations_ 2.13, pp. 143-162.
#' \doi{10.1007/s11558-018-9301-z}
#' }
#' \item{TOTA: }{
#' Alschner, Wolfgang, Julia Seiermann, and Dmitriy Skougarevskiy. 2017.
#' "Text-as-data analysis of preferential trade agreements: Mapping the PTA landscape".
#' UNCTAD Research Paper No. 5.
#' \url{https://github.com/mappingtreaties/tota.git}.
#' }
#' \item{GPTAD: }{
#' World Bank Group. 2014.
#' "Global Preferential Trade Agreement Database (GPTAD)".
#' Online database. _World Bank Group_.
#' \url{https://wits.worldbank.org/gptad/library.aspx}
#' }
#' }
#' @section Mapping:
#' 
#' |  *manytreaties*  | *IEADB*  | *HEIDI* | *TFDD* | *TREND* | *LABPTA* | *TOTA* | *GPTAD* |
#' |:-----------------|:---------|:--------|:-------|:--------|:---------|:-------|:--------|
#' | Title  | Treaty Name | Name.of.the.agreement | DocumentName| Trade.Agreement | Name | name | Common.Name |
#' | Signature  | Signature Date | signature.date | DateSigned | Year | year | date_signed | Date.of.Signature |
#' | Force  | Date IEA entered into force | | | Year | year | date_into_force | Date.of.Entry.into.Force | 
#' | mitchID  | IEA# | | | | | |
#' | heidiID  | | ID | | | | |
#' | labptaID | | | | | Number | |
#' | tfddID | | | 2016Update ID | | | |
#' | AgreementType  | Agreement Type (level 2) | | DocType | | | |
#' | Ambit  | Inclusion | | NumberParties | | | | Type |
#' | GeogArea | | | GeoScope | | | |
#' | Basin | | | Basin Name | | | |
#' | Issue | | | Issue Area | | | |
#' | Lineage | | | PrimaryTFDDID | | | |
#' 
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(agreements, manydata::mreport)
#' ```
"agreements"
