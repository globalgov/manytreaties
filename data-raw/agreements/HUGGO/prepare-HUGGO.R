# HUGGO Preparation Script

# The HUGGO dataset builds one existing datasets in the
# environment, trade, and health domains,
# improving on the precision of dates (eg. Signature, Entry into Force),
# resolving any conflicts between these datasets
# for the Title, Signature, Force dates,
# and filling in any missing information that could be found.
# For agreements that remain in force, the End date is coded as '9999-12-31'.
# Texts for the agreements are formatted and stored in .txt files in the
# 'data_raw/agreements/HUGGO/TreatyText' folder.
# The TreatyText variable denotes if the corresponding text
# has been stored in the folder.
# Below the ways in which the HUGGO dataset has been manipulated and 
# cleaned automatically are recorded.
# The 'data_raw/agreements/HUGGO/' folder also includes the files
# for the hand-coded and cleaned data.

### Environmental agreements

# Stage one: Collecting data
HUGGO1 <- readr::read_csv("data-raw/agreements/HUGGO/EnvGov Nodes-Table 1 VERS2.csv")
HUGGO2 <- readr::read_csv("data-raw/agreements/HUGGO/gnevar.csv")
HUGGO4 <- readr::read_csv("data-raw/agreements/HUGGO/GENG v1.2 (31.10.2015).csv")
HUGGO6 <- readr::read_delim("data-raw/agreements/HUGGO/MEA.Nodes v1.0.csv",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'HUGGO' object until the object created
# below (in stage four) passes all the tests.
HUGGO1 <- as_tibble(HUGGO1)  %>%
  dplyr::mutate(AgreementType = dplyr::recode(T, G = "A", M = "E", "T" = "Q",
                                              D = "V", R = "W", N = "X",
                                              U = "Y")) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocSign),
                        End = messydates::as_messydate(DocEnd),
                        Force = messydates::as_messydate(DocForce),
                        gengID = GENG) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::rename(DocType = L) %>%
  dplyr::rename(GeogArea = J) %>%
  dplyr::select(gengID, Title, Beg, End, DocType, GeogArea, AgreementType,
                Signature, Force) %>%
  dplyr::arrange(Beg)

# Add treatyID column
HUGGO1$treatyID <- manypkgs::code_agreements(HUGGO1)

# Clean HUGGO 2
HUGGO2 <- as_tibble(HUGGO2) %>%
  dplyr::mutate(AgreementType = dplyr::recode(T, G = "A", M = "E", "T" = "Q",
                                              D = "V", R = "W", N = "X",
                                              U = "Y")) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocSign),
                        End = messydates::as_messydate(DocEnd),
                        Force = messydates::as_messydate(DocForce),
                        gengID = GENG) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::rename(DocType = L) %>%
  dplyr::rename(GeogArea = J) %>%
  dplyr::select(gengID, Title, Beg, End, DocType, GeogArea, AgreementType,
                Signature, Force) %>%
  dplyr::arrange(Beg)

# Add treatyID column
HUGGO2$treatyID <- manypkgs::code_agreements(HUGGO2)

# Clean HUGGO4
HUGGO4$Parties <- paste0(HUGGO4$Country.x, "-", HUGGO4$Country.y)
HUGGO4 <- as_tibble(HUGGO4) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocDate),
                        Force = messydates::as_messydate(InForce),
                        gengID = `GENG-B`) %>%
  dplyr::mutate(End = messydates::as_messydate(End)) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::select(gengID, Title, Beg, Signature, Force, End, Parties)

# Add treatyID column
HUGGO4$treatyID <- manypkgs::code_agreements(HUGGO4)

# MEA nodes (MGENG dataset) - HUGGO6
# For some more information about the variables and codes,
# please see the documentation in the data-raw folder.
HUGGO6 <- as_tibble(HUGGO6) %>%
  dplyr::select(-'...1') %>%
  dplyr::rename(verified = '...2',
                ecolexID = ECOLEX.ID,
                ieaID = IEA.ID,
                gengID = GENG.ID,
                MEA_type = Type,
                subject_ecolex = Subject.x,
                subject_iea = Subject.y,
                url = Text) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocSign),
                        Force = messydates::as_messydate(DocForce)) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title),
                verified = case_when(verified == "%" ~ "verified",
                                     verified == "?" ~ "not verified"),
                Coded = as.character(Coded),
                Lit = as.character(Lit),
                Data = as.character(Data)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Beg)

# Add treatyID column
HUGGO6$treatyID <- manypkgs::code_agreements(HUGGO6)

# Join the datasets together
## fixed minor coding issue with HUGGO before joining data (28-09-2022)
# HUGGO$Source <- as.character(HUGGO$Source)

# Create a HUGGO_env "database" to apply consolidate() to match data on the same agreements
HUGGO_env <- list(HUGGO1, HUGGO2, HUGGO4, HUGGO6)

# Join the datasets together
HUGGO_env <- manydata::consolidate(HUGGO_env, row = "any", cols = "any",
                                   resolve = "coalesce", key = "treatyID") %>%
  dplyr::distinct()

# Add manyID column
manyID <- manypkgs::condense_agreements(manytreaties::agreements)
HUGGO_env <- dplyr::left_join(HUGGO_env, manyID, by = "treatyID")

# Stage three: Verify data and merge verified data and additional treaties into HUGGO_env dataset

# Load data frame with verified metadata (of treaties present in HUGGO)
HUGGO7 <- read.csv("data-raw/agreements/HUGGO/HUGGO_env_reconciled.csv")

# Load data frame with verified metadata (of treaties not present in HUGGO)
HUGGO8 <- read.csv("data-raw/agreements/HUGGO/HUGGO_env_additional.csv")

# Create one data frame
HUGGO9 <- rbind(HUGGO7, HUGGO8)

# Drop text columns and the ones included for verification purposes
HUGGO9 <- HUGGO9 %>%
  dplyr::select(-c(Source, Checked_HUGGO, Confirmed_HUGGO, Changes, Modified))

# Original data from HUGGO
# Detect if any row has non-ASCII characters in manyID and
# replace it with the corresponding treatyID
HUGGO_env[which(stringr::str_detect(HUGGO_env$manyID, "[^[:ascii:]]")), 1] <-
  HUGGO_env[which(stringr::str_detect(HUGGO_env$manyID, "[^[:ascii:]]")), 13]

# Verified and additional data
# Detect if any row has non-ASCII characters in manyID and
# replace it with the corresponding treatyID
HUGGO9[which(stringr::str_detect(HUGGO9$manyID, "[^[:ascii:]]")), 1] <-
  HUGGO9[which(stringr::str_detect(HUGGO9$manyID, "[^[:ascii:]]")), 13]

# Merge data frames
HUGGO_env <- dplyr::full_join(HUGGO_env, HUGGO9,
                              by = c("manyID", "treatyID")) %>%
  dplyr::distinct() %>%
  dplyr::relocate(manyID, Title.x, Title.y, Beg.x, Beg.y, Signature.x,
                  Signature.y, Force.x, Force.y, End.x, End.y, Parties.x,
                  Parties.y)

# Reformat merged data
HUGGO_env <- HUGGO_env %>%
  dplyr::mutate(Title = ifelse(!is.na(Title.y), Title.y, Title.x),
                Beg = ifelse(!is.na(Beg.y), Beg.y, Beg.x),
                Signature = ifelse(!is.na(Signature.y), Signature.y,
                                   Signature.x),
                End = ifelse(!is.na(End.y), End.y, End.x),
                Force = ifelse(!is.na(Force.y), Force.y, Force.x),
                url = ifelse(!is.na(url.y), url.y, url.x),
                Parties = ifelse(!is.na(Parties.y), Parties.y, Parties.x),
                AgreementType = ifelse(!is.na(AgreementType.y), AgreementType.y,
                                       AgreementType.x),
                DocType = ifelse(!is.na(DocType.y), DocType.y, DocType.x),
                GeogArea = ifelse(!is.na(GeogArea.y), GeogArea.y, GeogArea.x),
                ieaID = ifelse(!is.na(ieaID.y), ieaID.y, ieaID.x),
                gengID = ifelse(!is.na(gengID.y), gengID.y, gengID.x),
                ecolexID = ifelse(!is.na(ecolexID.y), ecolexID.y, ecolexID.x),
                verified = ifelse(!is.na(verified.y), verified.y, verified.x),
                DocValidUntilDate = ifelse(!is.na(DocValidUntilDate.y),
                                           DocValidUntilDate.y,
                                           DocValidUntilDate.x),
                Notes = ifelse(!is.na(Notes.y), Notes.y, Notes.x),
                Download = ifelse(!is.na(Download.y), Download.y, Download.x),
                MEA_type = ifelse(!is.na(MEA_type.y), MEA_type.y, MEA_type.x),
                Ambit = ifelse(!is.na(Ambit.y), Ambit.y, Ambit.x),
                Region = ifelse(!is.na(Region.y), Region.y, Region.x),
                subject_ecolex = ifelse(!is.na(subject_ecolex.y),
                                        subject_ecolex.y, subject_ecolex.x),
                subject_iea = ifelse(!is.na(subject_iea.y), subject_iea.y,
                                     subject_iea.x),
                Keywords = ifelse(!is.na(Keywords.y), Keywords.y, Keywords.x),
                Lineage = ifelse(!is.na(Lineage.y), Lineage.y, Lineage.x),
                Sequence = ifelse(!is.na(Sequence.y), Sequence.y, Sequence.x),
                AdoptedIn = ifelse(!is.na(AdoptedIn.y), AdoptedIn.y,
                                   AdoptedIn.x),
                Languages = ifelse(!is.na(Languages.y), Languages.y,
                                   Languages.x),
                Appendices = ifelse(!is.na(Appendices.y), Appendices.y,
                                    Appendices.x),
                Depository = ifelse(!is.na(Depository.y), Depository.y,
                                    Depository.x),
                DepositoryURL = ifelse(!is.na(DepositoryURL.y), DepositoryURL.y,
                                       DepositoryURL.x),
                Published = ifelse(!is.na(Published.y), Published.y,
                                   Published.x),
                Abstract = ifelse(!is.na(Abstract.y), Abstract.y, Abstract.x),
                Website1 = ifelse(!is.na(Website1.y), Website1.y, Website1.x),
                Website2 = ifelse(!is.na(Website2.y), Website2.y, Website2.y),
                Secretariat = ifelse(!is.na(Secretariat.y), Secretariat.y,
                                     Secretariat.x),
                SecretariatURL = ifelse(!is.na(SecretariatURL.y),
                                        SecretariatURL.y, SecretariatURL.x),
                UNEP = ifelse(!is.na(UNEP.y), UNEP.y, UNEP.x),
                Supersedes = ifelse(!is.na(Supersedes.y), Supersedes.y,
                                    Supersedes.x),
                References = ifelse(!is.na(References.y), References.y,
                                    References.x),
                EnabledBy = ifelse(!is.na(EnabledBy.y), EnabledBy.y,
                                   EnabledBy.x),
                AmendedBy = ifelse(!is.na(AmendedBy.y), AmendedBy.y,
                                   AmendedBy.x),
                Lit = ifelse(!is.na(Lit.y), Lit.y, Lit.x),
                Data = ifelse(!is.na(Data.y), Data.y, Data.x),
                Coded = ifelse(!is.na(Coded.y), Coded.y, Coded.x))%>%
  dplyr::distinct() %>%
  dplyr::select(-c(Title.x, Title.y, Beg.x, Beg.y, End.x, End.y, Signature.x,
                   Signature.y, Force.x, Force.y, Abstract.x, Abstract.y,
                   Parties.x, Parties.y, AgreementType.x, AgreementType.y,
                   DocType.y, DocType.x, GeogArea.x, GeogArea.y, gengID.x,
                   gengID.y, ieaID.x, ieaID.y, ecolexID.x, ecolexID.y,
                   verified.x, verified.y, DocValidUntilDate.x,
                   DocValidUntilDate.y, url.x, url.y, Notes.x, Notes.y,
                   Download.x, Download.y, MEA_type.x, MEA_type.y, Ambit.x,
                   Ambit.y, Region.x, Region.y, subject_ecolex.x,
                   subject_ecolex.y, subject_iea.x, subject_iea.y,
                   Keywords.x, Keywords.y, Lineage.x, Lineage.y, Sequence.x,
                   Sequence.y, AdoptedIn.x, AdoptedIn.y, Languages.x,
                   Languages.y, Appendices.x, Appendices.y, Depository.x,
                   Depository.y, DepositoryURL.x, DepositoryURL.y, Published.x,
                   Published.y, Website1.x, Website1.y, Website2.x, Website2.y,
                   Secretariat.x, Secretariat.y, SecretariatURL.x,
                   SecretariatURL.y, UNEP.x, UNEP.y,Supersedes.x, Supersedes.y,
                   References.x, References.y, EnabledBy.x, EnabledBy.y,
                   AmendedBy.x, AmendedBy.y, Lit.x, Lit.y, Data.x, Data.y,
                   Coded.x, Coded.y)) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title)) %>%
  dplyr::relocate(manyID, Title, Beg, End, Signature, Force, url,
                  AgreementType, DocType, GeogArea, gengID, ieaID, ecolexID,
                  treatyID, Parties, verified, DocValidUntilDate, Notes,
                  Download, MEA_type, Ambit, Region, subject_ecolex,
                  subject_iea, Keywords, Lineage, Sequence, AdoptedIn,
                  Languages, Appendices, Depository, DepositoryURL,
                  Published, Website1, Website2, Secretariat, SecretariatURL,
                  UNEP, Supersedes, References, EnabledBy, AmendedBy, Lit,
                  Data, Coded, Abstract) %>%
  dplyr::arrange(Beg) %>%
  dplyr::rename(Begin = Beg)

# Stage four: Matching agreements with titles in different languages
noneng <- HUGGO_env %>%
  # subset agreements with non-English titles but match a row with an English title
  dplyr::filter(manyID == "UB08IB_1893A" | manyID == "ESP-FRA[DGP]_1900A" |
                  manyID == "ED19DL_1905A" | manyID == "CHE-FRA[DDK]_1930A" |
                  manyID == "ARG-PRY[DRP]_1939A" | manyID == "CP09RP_1971A" |
                  manyID == "CP09RP_1971A" | manyID == "TRTDDY_1973A" |
                  manyID == "AS13DH_1978A" | manyID == "AT24JD_1979A" |
                  manyID == "AD25ED_1979A" | manyID == "MAR-ROU[SDR]_1979A" |
                  manyID == "BEL-LUX[EDL]_1980A" | manyID == "COL-ECU[SPA]_1994A" |
                  manyID == "FRA-NOR[PLM]_1995A" | manyID == "AS32AS_1995A" |
                  manyID == "AM17LS_1996A" | manyID == "ARG-PRY[LPN]_1996A" |
                  manyID == "AE21LM_1997A" | manyID == "AM17LD_1997A" |
                  manyID == "ARG-BOL[RDB]_1998A" | manyID == "TD13DP_1998A" |
                  manyID == "AP17RT_2002A" | manyID == "CHN-TUN[DTM]_2002A" |
                  manyID == "DZA-TUR[RDT]_2002A" | manyID == "AM23AE_2002A" |
                  manyID == "AE38LD_2002A" | manyID == "AE19DL_2003A" |
                  manyID == "AE24LJ_2003A"| manyID == "EL50LJ_2004A" |
                  manyID == "ARG-BOL[RDB]_2004P" | manyID == "AB22LA_2005A" |
                  manyID == "HND-MEX[EUM]_2005A" | manyID == "CHL-DZA[RDC]_2005A" |
                  manyID == "DZA-PER[RDP]_2005A" | manyID == "AC46LJ_2005A" |
                  manyID == "CD34RM_2005N1" | manyID == "MD19CC_2005S" |
                  manyID == "AM15DC_2006A" | manyID == "AC43LO_2006A" |
                  manyID == "NIC-VEN[LRD]_2007A" | manyID == "AC33LJ_2007A"|
                  manyID == "ARG-ECU[LHE]_2007A" | manyID == "AC11PB_2007A" |
                  manyID == "AD33LN_2007A" | manyID == "AC34LD_2007A" |
                  manyID == "AD25LF_2008A" | manyID == "BRA-CRI[DSL]_2008A"|
                  manyID == "BRA-CRI[PDB]_2008A" | manyID == "BRA-CRI[DCE]_2008P" |
                  manyID == "AE22LE_2008A" | manyID == "AS35LO_2008A" |
                  manyID == "CP09LA_2012O"| manyID == "CC09DM_1954A" |
                  manyID == "VB13GV_2008O" | manyID == "CS18PS_1954A"|
                  manyID == "CS18PS_1954A")
noneng <- noneng %>%
  dplyr::rename(match = manyID, Orig_noneng_title = Title) %>%
  dplyr::arrange(match) %>%
  # add in matching list of manyIDs of agreements with English titles
  dplyr::mutate(manyID = c("FRA-URY[CCM]_2005A", "SI04DB_2007P", "FRA-USA[PPP]_2007A",
                           "UKR-ALG[PPA]_2007A", "FRA-TUN[CCT]_2006P:UNFCCC_1992A",
                           "BRA-FRA[MKP]_2005P", "ESP-SEN[FAD]_1979A", "FRA-ZAF[ECT]_2008A",
                           "CHN-FRA[NEB]_2007A", "FRA-GAB[HWD]_2003A", "FRA-TUR[FEP]_1997A",
                           "VEN-ZAF[ENR]_2008A", "GF13JT_2003A", "FRA-KIR[EZT]_2002A",
                           "COG-ALG[MRD]_2006A", "FRA-LVA[MFR]_1997A", "FRA-TUR[MFP]_1996A",
                           "ARG-ALG[DSI]_2002A", "TUN-ALG[MBD]_2002A", "ARG-BOL[EIB]_1998A",
                           "ARG-BOL[ENB]_2004P", "ARG-ECU[SHE]_2007A", "ARG-PRY[LRP]_1939A",
                           "ARG-PRY[PNP]_1996A", "COL-HTI[MSA]_1978A", "CHE-FRA[LGP]_1995A",
                           "DEU-FRA[SFP]_2008A",
                           "ESP-MAR[MFR]_1979A","KB04WS_1980A","BRA-CRI[MCT]_2008P",
                           "BRA-CRI[ASL]_2008P","BRA-CRI[ABP]_2008P","CDSMZM_1954P20",
                           "ESP-PRT[ISM]_2005N","CHE-FRA[BDK]_1930A","CHL-ALG[EMD]_2005A",
                           "CHN-TUN[PMT]_2002A","COL-ECU[ARF]_1994A","ESTANA_2012A",
                           "STDYPR_1971A", "INVEPR_1971A", "ES06SP_1954A","PER-ALG[EMD]_2005A",
                           "TUR-ALG[END]_2002A","CF08CL_1905N11","FRA-MCO[TMS]_2004A",
                           "FS10PJ_1900A","FRA-NOR[SSP]_1995A","HND-MEX[MDM]_2005A",
                           "MAR-ROU[TMS]_1979A","ESP-GTM[ECC]_2005S","NIC-VEN[ENS]_2007A",
                           "ECU-PER[CMG]_1998A","YCYRTT_1973A","AH12IF_1893A", "TTICPO_2008A"
  ))
noneng <- dplyr::select(noneng, manyID, Orig_noneng_title, Begin, match)
# Non-English titles of the same agreement with an English title added in variable 'Orig_noneng_title'
# manyID of the non-English agreement added in variable 'match'
HUGG_env <- dplyr::left_join(HUGGO_env, noneng, by = c("manyID", "Begin"))
# remove rows with non-English titles identified above
HUGGO_env <- HUGGO_env[-which(HUGGO_env$manyID == "UB08IB_1893A" | HUGGO_env$manyID == "ESP-FRA[DGP]_1900A" |
                         HUGGO_env$manyID == "ED19DL_1905A" | HUGGO_env$manyID == "CHE-FRA[DDK]_1930A" |
                         HUGGO_env$manyID == "ARG-PRY[DRP]_1939A" | HUGGO_env$manyID == "CP09RP_1971A" |
                         HUGGO_env$manyID == "CP09RP_1971A" | HUGGO_env$manyID == "TRTDDY_1973A" |
                         HUGGO_env$manyID == "AS13DH_1978A" | HUGGO_env$manyID == "AT24JD_1979A" |
                         HUGGO_env$manyID == "AD25ED_1979A" | HUGGO_env$manyID == "MAR-ROU[SDR]_1979A" |
                         HUGGO_env$manyID == "BEL-LUX[EDL]_1980A" | HUGGO_env$manyID == "COL-ECU[SPA]_1994A" |
                         HUGGO_env$manyID == "FRA-NOR[PLM]_1995A" | HUGGO_env$manyID == "AS32AS_1995A" |
                         HUGGO_env$manyID == "AM17LS_1996A" | HUGGO_env$manyID == "ARG-PRY[LPN]_1996A" |
                         HUGGO_env$manyID == "AE21LM_1997A" | HUGGO_env$manyID == "AM17LD_1997A" |
                         HUGGO_env$manyID == "ARG-BOL[RDB]_1998A" | HUGGO_env$manyID == "TD13DP_1998A" |
                         HUGGO_env$manyID == "AP17RT_2002A" | HUGGO_env$manyID == "CHN-TUN[DTM]_2002A" |
                         HUGGO_env$manyID == "DZA-TUR[RDT]_2002A" | HUGGO_env$manyID == "AM23AE_2002A" |
                         HUGGO_env$manyID == "AE38LD_2002A" | HUGGO_env$manyID == "AE19DL_2003A" |
                         HUGGO_env$manyID == "AE24LJ_2003A"| HUGGO_env$manyID == "EL50LJ_2004A" |
                         HUGGO_env$manyID == "ARG-BOL[RDB]_2004P" | HUGGO_env$manyID == "AB22LA_2005A" |
                         HUGGO_env$manyID == "HND-MEX[EUM]_2005A" | HUGGO_env$manyID == "CHL-DZA[RDC]_2005A" |
                         HUGGO_env$manyID == "DZA-PER[RDP]_2005A" | HUGGO_env$manyID == "AC46LJ_2005A" |
                         HUGGO_env$manyID == "CD34RM_2005N1" | HUGGO_env$manyID == "MD19CC_2005S" |
                         HUGGO_env$manyID == "AM15DC_2006A" | HUGGO_env$manyID == "AC43LO_2006A" |
                         HUGGO_env$manyID == "NIC-VEN[LRD]_2007A" | HUGGO_env$manyID == "AC33LJ_2007A"|
                         HUGGO_env$manyID == "ARG-ECU[LHE]_2007A" | HUGGO_env$manyID == "AC11PB_2007A" |
                         HUGGO_env$manyID == "AD33LN_2007A" | HUGGO_env$manyID == "AC34LD_2007A" |
                         HUGGO_env$manyID == "AD25LF_2008A" | HUGGO_env$manyID == "BRA-CRI[DSL]_2008A"|
                         HUGGO_env$manyID == "BRA-CRI[PDB]_2008A" | HUGGO_env$manyID == "BRA-CRI[DCE]_2008P" |
                         HUGGO_env$manyID == "AE22LE_2008A" | HUGGO_env$manyID == "AS35LO_2008A" |
                         HUGGO_env$manyID == "CP09LA_2012O"| HUGGO_env$manyID == "CC09DM_1954A" |
                         HUGGO_env$manyID == "VB13GV_2008O" | HUGGO_env$manyID == "CS18PS_1954A"|
                         HUGGO_env$manyID == "CS18PS_1954A"),]

# Stage five: Correct coding for variables

# Recode "9999-XX-XX-" Force date for PF07BS_1897A, RM04LF_1937A,
# IP07TS_1971E, CPRAEM_2005P:CPRAEM_1999A, EF04GG_2007A
# since this coding is no longer used for Force dates in HUGGO_env dataset.
HUGGO_env <- HUGGO_env %>%
  dplyr::mutate(Force = ifelse(grepl("9999-XX-XX-", Force), NA, Force))

# Recode 'NA' values for TreatyText
HUGGO_env <- HUGGO_env %>%
  dplyr::mutate(TreatyText = ifelse(is.na(TreatyText), 0, TreatyText))

### Trade agreements

# Stage one: Collecting data

# Import hand-coded verified data
HUGGO_reconciled <- readr::read_csv("data-raw/agreements/HUGGO/HUGGO_trd_reconciled.csv")
HUGGO_additional <- readr::read_csv("data-raw/agreements/HUGGO/HUGGO_trd_additional.csv")

HUGGO_trd <- dplyr::bind_rows(HUGGO_reconciled, HUGGO_additional)

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'HUGGO' object until the object created
# below (in stage three) passes all the tests.

# Add updated 'Begin' column
HUGGO_trd <- HUGGO_trd %>%
  dplyr::mutate(Begin = dplyr::coalesce(Signature, Force))

# Standardise class of date variables ('mdate')
HUGGO_trd <- HUGGO_trd %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End))

# Add new TreatyText column to mark status of collection of texts
# TreatyText: 0 = No text collected, 1 = Treaty text collected in
# 'data-raw/agreements/HUGGO/TreatyTexts' folder
HUGGO_trd$TreatyText <- 0
HUGGO_trd$TreatyText[is.na(HUGGO$No_source)] <- 1

# Add treatyID and manyID for new entries
HUGGO_trd$treatyID <- manypkgs::code_agreements(HUGGO, HUGGO$Title, HUGGO$Begin)
manyID <- manypkgs::condense_agreements(manytreaties::agreements)
HUGGO_trd <- dplyr::left_join(HUGGO_trd, manyID, by = "treatyID")
HUGGO_trd <- HUGGO_trd %>%
  dplyr::mutate(manyID = ifelse(!is.na(manyID.x), manyID.x, manyID.y)) %>%
  dplyr::select(-c(manyID.x, manyID.y)) %>%
  dplyr::relocate(manyID, treatyID)

# Reorder columns and arrange observations by 'Begin' variable for export
HUGGO_trd <- HUGGO_trd %>%
  dplyr::select(manyID, Title, Begin, Signature, Force, End,
                url, Citation, TreatyText, treatyID, Coder) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Begin)

### Health agreements

# Stage one: Assembling data from existing datasets to identify conflicts
HUGGO_original <- manydata::consolidate(manyhealth::agreements,
                                        "any",
                                        "any",
                                        "coalesce",
                                        "manyID") %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin))

# Stage two: Resolving conflicts
overlap <- manydata::compare_overlap(manyhealth::agreements,
                                       key = c("Title", "Begin"))

# Stage three: load and clean verified data to export as HUGGO dataset
# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.
HUGGO_hlt <- readr::read_csv("data-raw/agreements/HUGGO/HUGGO_hlt_reconciled.csv")
HUGGO_hlt <- HUGGO_hlt %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title),
                Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End))

## Added 'Formal' variable identifying legally-binding formal agreements
## Formal = 1 indicates the agreement is legally-binding.
## 'Dataset' variable indicates the dataset in which agreement is also found
## (WHO, GHHR, or Both)
## 'Source' variable indicates the source of the agreement:
## WHO, UN, OAS (Organization of American States), AU (African Union),
## ACHPR (African Commission of Human and Peoples' Rights),
## COE (Council of Europe), EU (European Union), WMA (World Medical Association),
## WPA (World Psychiatric association), PAHO (Pan-American Health Organization)
## 'Health_as_primary_intent' variable indicates if agreement is primarily
## intended to govern health issues; either Y (Yes) or N (No)

# make sure manyIDs and treatyIDs are updated
HUGGO_hlt$treatyID <- manypkgs::code_agreements(HUGGO_hlt, HUGGO_hlt$Title, HUGGO_hlt$Begin)
manyID <- manypkgs::condense_agreements(manytreaties::agreements)
HUGGO_hlt <- dplyr::left_join(HUGGO_hlt, manyID, by = "treatyID")
HUGGO_hlt <- HUGGO_hlt %>%
  manydata::transmutate(manyID = ifelse(!is.na(manyID.y), manyID.y, manyID.x)) %>%
  dplyr::distinct()
HUGGO_hlt <- HUGGO_hlt %>%
  dplyr::relocate(manyID, treatyID, Title, Begin, Signature, Force, End) %>%
  dplyr::select(-c(Health_in_scope, Type))

# Stage four: Improve Topic variable
## Improve Topic variable identifying issue of agreements.
## ___ issues are identified and coded here:
# compare with Topic and Lineage variables
# Currently, 9 topics are coded from the agreement titles:
# Labour, Human Rights, Protection, Mental Health, Prevention, Diseases,
# Healthcare, Pollution, Climate change
HUGGO_hlt$Topic <- manypkgs::code_domain(HUGGO_hlt$Title, type  = "health")

### Merge data on environmental, trade, and health agreements together
HUGGO_env <- HUGGO_env %>%
  dplyr::mutate(Domain = "Environment")
HUGGO_trd <- HUGGO_trd %>%
  dplyr::mutate(Domain = "Trade")
HUGGO_hlt <- HUGGO_hlt %>%
  dplyr::mutate(Domain = "Health")
HUGGO <- dplyr::bind_rows(HUGGO_env, HUGGO_trd, HUGGO_hlt)

# Combine data for rows on the same agreement


# Format data correctly for exporting
HUGGO <- HUGGO %>%
  dplyr::mutate(across(everything(),
                       ~stringr::str_replace_all(., "^NA$", NA_character_))) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End)) %>%
  dplyr::arrange(Begin) %>%
  dplyr::relocate(Coder, .after = last_col()) %>%
  dplyr::relocate(manyID, treatyID, Title, Begin, End, Signature, Force,
                  TreatyText, url, Domain)

# Stage seven: Connecting data
# Next run the following line to make HUGGO available
# within the package.
manypkgs::export_data(HUGGO, datacube = "agreements",
                      URL = "Hand-coded data by the GGO team")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure
# adherence to certain standards. You can hit Cmd-Shift-T (Mac)
# or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and
# fill in as much detail about the variables etc as possible.
