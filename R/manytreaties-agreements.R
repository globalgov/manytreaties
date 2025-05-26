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
#' \item{DESTA: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$DESTA), big.mark=",")` 
#'   observations and `r ncol(manytreaties::agreements$DESTA)` variables: 
#'   `r cli::pluralize("{names(manytreaties::agreements$DESTA)}")`.}
#' \item{TFDD: }{A dataset with `r prettyNum(nrow(manytreaties::agreements$TFDD), big.mark=",")`
#'   observations and `r ncol(manytreaties::agreements$TFDD)` variables:
#'   `r cli::pluralize("{names(manytreaties::agreements$TFDD)}")`.}
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
#' }
#' For more information and references to each of the datasets used,
#' please use the `manydata::call_sources()` and `manydata::compare_dimensions()` functions.
#' @source
#'   `r call_citations(agreements, output = "help")`
#' @section Mapping:
#' 
#' |  *manytreaties*  | *IEADB*  | *HEIDI* | *DESTA* | *TFDD* | *TREND* | *LABPTA* | *TOTA* | *GPTAD* |
#' |:-----------------|:---------|:--------|:--------|:-------|:--------|:---------|:-------|:--------|
#' | Title  | Treaty Name | Name.of.the.agreement | name | DocumentName| Trade.Agreement | Name | name | Common.Name |
#' | Signature  | Signature Date | signature.date | year | DateSigned | Year | year | date_signed | Date.of.Signature |
#' | Force  | Date IEA entered into force | | entryforceyea | | Year | year | date_into_force | Date.of.Entry.into.Force | 
#' | mitchID  | IEA# | | | | | | |
#' | heidiID  | | ID | | | | | |
#' | destaID | | | base_treaty | | | | |
#' | labptaID | | | | | Number | | |
#' | tfddID | | | 2016Update ID | | | | |
#' | AgreementType  | Agreement Type (level 2) | | entry_type | DocType | | | |
#' | Ambit  | Inclusion | | typememb | NumberParties | | | | Type |
#' | GeogArea | | | regioncon | GeoScope | | | |
#' | Basin | | | | Basin Name | | | |
#' | Issue | | | | Issue Area | | | |
#' | Lineage | | | | PrimaryTFDDID | | | |
#' 
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(agreements, manydata::mreport)
#' ```
"agreements"

info_agreements <- tibble::tibble(Dataset = names(data(agreements, package = "manytreaties")),
                                   Source = c("Hollway, James, Henrique Sposito, and Jael Tan. 2021. International agreements for manydata.",
                                              "Mitchell, Ron B. et al. 2020. 'What we know (and could know) about international environmental agreements'. _Global Environmental Politics_ 20.1, pp. 103-121.",
                                              "Morin, Jean-Frederic, and Chantal Blouin. 2019. 'How environmental treaties contribute to global health governance'. _Globalization and health_ 15.1, pp. 1-8.",
                                              "Dür, Andreas, Leonardo Baccini, and Manfred Elsig. 2014. 'The Design of International Trade Agreements: Introducing a New Database'. _The Review of International Organizations_ 9.3, pp. 353-375.",
                                              "Oregon College of Earth and Oregon State University Atmospheric Science. 2021. _Product of the Transboundary Freshwater Dispute Database_.",
                                              "Morin, Jean-Frederic, Andreas Duer, and Lisa Lechner. 2018. 'Mapping the trade and environment nexus: Insights from a new dataset'. _Global Environmental Politics_ 18.1, pp. 122-139.",
                                              "Raess, Damien, Andreas Duer, and D. Sari. 2018. 'Protecting labor rights in preferential trade agreements: The role of trade unions, left governments, and skilled labor'. _The Review of International Organizations_ 2.13, pp. 143-162.",
                                              "Alschner, Wolfgang, Julia Seiermann, and Dmitriy Skougarevskiy. 2017. 'Text-as-data analysis of preferential trade agreements: Mapping the PTA landscape'. UNCTAD Research Paper No. 5.",
                                              "World Bank Group. 2014. _Global Preferential Trade Agreement Database (GPTAD)_. Online database. World Bank Group."),
                                      URL = c("",
                                              "https://www.iea.ulaval.ca/en/country-members",
                                              "https://www.chaire-epi.ulaval.ca/en/data/heidi",
                                              "https://www.designoftradeagreements.org/downloads/",
                                              "http://transboundarywaters.science.oregonstate.edu",
                                              "http://www.chaire-epi.ulaval.ca/en/trend",
                                              "https://doi.org/10.1007/s11558-018-9301-z",
                                              "https://github.com/mappingtreaties/tota.git",
                                              "https://wits.worldbank.org/gptad/library.aspx"))

