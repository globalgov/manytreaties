#' agreements datacube documentation
#'
#' @format The agreements datacube is a list that contains the
#' following 9 datasets: GHHR, GPTAD, HEIDI, IEADB, LABPTA, TOTA, TREND, WHO, HUGGO.
#' For more information and references to each of the datasets used,
#' please use the `manydata::call_sources()` and `manydata::compare_dimensions()` functions.
#'\describe{
#' \item{GHHR: }{A dataset with 149 observations and the following
#' 8 variables: manyID, treatyID, Title, Begin, Region, LegalStatus, Lineage,
#' ghhrID.}
#' \item{GPTAD: }{A dataset with 340 observations and the following
#' 10 variables: manyID, treatyID, Title, Begin, AgreementType, DocType,
#' GeogArea, Signature, Force, gptadID.}
#' \item{HEIDI: }{A dataset with 2280 observations and the following
#' 7 variables: manyID, treatyID, Title, Begin, Signature, Lineage, heidiID.}
#' \item{IEADB: }{A dataset with 3667 observations and the following
#' 10 variables: manyID, treatyID, Title, Begin, DocType, AgreementType,
#' Signature, Force, Lineage, ieadbID.}
#' \item{LABPTA: }{A dataset with 483 observations and the following
#' 7 variables: manyID, treatyID, Title, Begin, Signature, Force, labptaID.}
#' \item{TOTA: }{A dataset with 442 observations and the following
#' 7 variables: manyID, treatyID, Title, Begin, Signature, Force, totaID.}
#' \item{TREND: }{A dataset with 710 observations and the following
#' 7 variables: manyID, treatyID, Title, Begin, Signature, Force, trendID.}
#' \item{WHO: }{A dataset with 114 observations and the following
#' 6 variables: manyID, treatyID, Title, Begin, Organisation, Topic.}
#' \item{HUGGO: }{A dataset with 4816 observations and the following
#' 59 variables: manyID, treatyID, Title, Begin, End, Signature, Force,
#' TreatyText, url, Domain, AgreementType, DocType, GeogArea, gengID, ieaID,
#' ecolexID, Parties, verified, DocValidUntilDate, Notes, Download, MEA_type,
#' Ambit, Region, subject_ecolex, subject_iea, Keywords, Lineage, Sequence,
#' AdoptedIn, Languages, Appendices, Depository, DepositoryURL, Published,
#' Website1, Website2, Secretariat, SecretariatURL, UNEP, Supersedes,
#' References, EnabledBy, AmendedBy, Lit, Data, Coded, Abstract, Language,
#' Orig_noneng_title, match, Citation, Formal, Dataset, Source,
#' Health_as_primary_intent, Comments, Topic, Coder.}
#' }
#' @source
#' \itemize{
#' \item{GHHR: }{
#' B. M. Meier, O. A. Cabrera, A. Ayala, et al.
#' “Bridging international law and rights-based litigation:mapping health-related rights through the development of the global health and human rights database”.
#' _Health & Human Rights._ 14 (2012), p. 20.}
#' \item{GPTAD: }{
#' World Bank Group. _Global Preferential Trade Agreement Database (GPTAD)_.
#' Online database. publisher: World Bank Group. 2014.
#' <https://wits.worldbank.org/gptad/library.aspx>.}
#' \item{HEIDI: }{
#' J-F. Morin, and C. Blouin. "How environmental treaties contribute to global health governance".
#' _Globalization and health_ 15.1 (2019), pp. 1-8.
#' }
#' \item{IEADB: }{
#' R. B. Mitchell et al. "What we know (and could know) about international environmental agreements".
#' _Global Environmental Politics_ 20.1 (2020), pp. 103-121.
#' }
#' \item{LABPTA: }{
#' D. Raess, A. Dür, and D. Sari.
#' “Protecting labor rights in preferential trade agreements: The role of trade unions, left governments, and skilled labor”.
#' In: _The Review of International Organizations_ 2.13(2018), pp. 143-162.
#' DOI: 10.1007/s11558-018-9301-z.}
#' \item{TOTA: }{
#' W. Alschner, J. Seiermann, and D. Skougarevskiy.
#' _Text-as-data analysis of preferential tradeagreements: Mapping the PTA landscape_.
#' 2017. <https://github.com/mappingtreaties/tota.git>.}
#' \item{TREND: }{
#' J. Morin, A. Dür, and L. Lechner.
#' “Mapping the trade and environment nexus: Insights from a newdataset”.
#' In: _Global Environmental Politics_ 18.1 (2018), pp. 122-139.}
#' \item{WHO: }{
#' World Health Organization.
#' _WHO MiNDbank: More Inclusiveness Needed in Disability and Development_. 
#' Online database. publisher: World Health Organization. 2024.
#' <https://extranet.who.int/mindbank/>.}
#' \item{HUGGO: }{
#' J. Hollway. International agreements for manydata. 2021.
#' }
#' }
#' @section URL:
#' \itemize{
#' \item{GHHR: }{
#' \url{https://www.globalhealthrights.org/instruments/instrument-region/}
#' }
#' \item{GPTAD: }{
#' \url{https://wits.worldbank.org/gptad/library.aspx}
#' }
#' \item{HEIDI: }{
#' \url{https://www.chaire-epi.ulaval.ca/en/data/heidi}
#' }
#' \item{IEADB: }{
#' \url{https://iea.uoregon.edu/base-agreement-list}
#' }
#' \item{LABPTA: }{
#' \url{https://doi.org/10.1007/s11558-018-9301-z}
#' }
#' \item{TOTA: }{
#' \url{https://github.com/mappingtreaties/tota.git}
#' }
#' \item{TREND: }{
#' \url{http://www.chaire-epi.ulaval.ca/en/trend}
#' }
#' \item{WHO: }{
#' \url{https://www.mindbank.info/collection/un_who_resolutions/all?page=all}
#' }
#' \item{HUGGO: }{Hand-coded data by the GGO team.}
#' }
#' @section Mapping:
#' \itemize{
#' \item{GHHR: }{
#' Variable Mapping
#'
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | Year of adoption | Begin |
#'
#' }
#' \item{GPTAD: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | Common.Name | Title |
#' | Date.of.Signature | Signature |
#' | Date.of.Entry.into.Force | Force |
#' | Type | DocType |
#' | Type | AgreementType |
#' | Type | GeogArea |
#' 
#' }
#' \item{HEIDI: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | ID | heidiID |
#' | 'Name.of.the.agreement' | Title |
#' | signature.date | Signature |
#' 
#' }
#' \item{IEADB: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | 'IEA# (click for add'l info)' | ieadbID |
#' | 'Treaty Name' | Title |
#' | 'Signature Date' | Signature |
#' | 'Date IEA entered into force' | Force |
#' | 'Agreement Type (level 2)' | AgreementType |
#' | Inclusion | DocType |
#' 
#' }
#' \item{LABPTA: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | Number | labptaID |
#' | Name | Title |
#' | year | Signature |
#' | year | Force |
#' 
#' }
#' \item{TOTA: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | name | Title |
#' | date_signed | Signature |
#' | date_into_force | Force |
#' 
#' }
#' \item{TREND: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | Trade.Agreement | Title |
#' | Year | Signature |
#' | Year | Force |
#' 
#' }
#' \item{WHO: }{
#' Variable Mapping
#'
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | NA | NA |
#'
#' }
#' \item{HUGGO: }{
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
#' lapply(agreements, messydates::mreport)
#' ```
"agreements"
