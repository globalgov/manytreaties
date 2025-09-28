# HUGGO_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# for the 'many' packages.

#### Environmental agreements
# Stage one: Assembling original data
HUGGO_MEM_env <- readr::read_csv("data-raw/memberships/HUGGO_MEM/gnevar.csv")

# Stage two: Correcting data
HUGGO_MEM_env <- as_tibble(HUGGO_MEM_env) %>%
  janitor::remove_empty(which = "cols") %>%
  dplyr::mutate(gengID = GENG,
                ecolexID = ECOLEX,
                ieaID = IEA,
                stateID = Country,
                stateRat = messydates::as_messydate(Approval),
                stateSignature = messydates::as_messydate(Signature),
                stateWithdrawal = messydates::as_messydate(Withdrawal1),
                stateWithdrawal2 = messydates::as_messydate(Withdrawal2),
                Signature = messydates::as_messydate(DocSign),
                Force = messydates::as_messydate(DocForce),
                Term = messydates::as_messydate(DocEnd),
                stateForce = messydates::as_messydate(InForce1),
                stateForce2 = messydates::as_messydate(InForce2),
                stateForce3 = messydates::as_messydate(InForce3),
                verified = case_when(X == "%" ~ "verified",
                                     X == "?" ~ "not verified"),
                Title = manypkgs::standardise_titles(Title),
                ProvisionalApp = messydates::as_messydate(ProvisionalApp),
                Deposit = messydates::as_messydate(Deposit),
                Begin = dplyr::coalesce(Signature, stateRat, stateForce),
                End = dplyr::coalesce(Term, stateWithdrawal)) %>%
  dplyr::select(stateID, Title, Begin, End, Signature, stateSignature,
                stateRat, Force, stateForce, stateForce2, stateForce3,
                Term, stateWithdrawal, stateWithdrawal2,
                gengID, ecolexID, ieaID, Comments, Deposit, obsolete,
                ProvisionalApp, Reservation, verified) %>%
  dplyr::arrange(Begin) %>%
  dplyr::distinct()

# Add treatyID column
HUGGO_MEM_env$treatyID <- manypkgs::code_agreements(HUGGO_MEM_env,
                                                    HUGGO_MEM_env$Title,
                                                    HUGGO_MEM_env$Begin)

# Add MEA edges data (MGENG dataset)
# For some more information about the variables and codes,
# please see the documentation in the data-raw folder.
MEA_edges <- readr::read_delim("data-raw/agreements/HUGGO/MEA.Edges v1.0.csv",
                               delim = ";", escape_double = FALSE,
                               trim_ws = TRUE)
# Let's wrangle some variables to keep consistent.
names(MEA_edges) <- gsub("\\.", "", names(MEA_edges))
names(MEA_edges) <- gsub("^Membership", "", names(MEA_edges))
names(MEA_edges) <- gsub("^dateOf", "", names(MEA_edges))
MEA_edges <- as_tibble(MEA_edges) %>%
  dplyr::select(-'1') %>%
  janitor::remove_empty(which = "cols") %>%
  manydata::transmutate(gengID = GENGID,
                        stateID = Country,
                        stateSignature = messydates::as_messydate(MembSignx),
                        stateRat = messydates::as_messydate(MembRatx),
                        stateForce_ecolex = messydates::as_messydate(MembForcex),
                        stateForce_iea = messydates::as_messydate(MembForcey),
                        stateForce2 = messydates::as_messydate(EntryintoForce2),
                        stateWithdrawal = messydates::as_messydate(Withdrawal),
                        ProvisionalApp = messydates::as_messydate(ProvisionalApplication),
                        Succession = messydates::as_messydate(Succession),
                        Deposit = messydates::as_messydate(DepositofInstrument),
                        CooperatingNonparty = messydates::as_messydate(CooperatingNonparty),
                        DefiniteSignature = messydates::as_messydate(DefiniteSignature),
                        Consent = messydates::as_messydate(ConsentToBeBound),
                        Acceptance = messydates::as_messydate(AcceptanceApproval),
                        Accession = messydates::as_messydate(AccessionApprobation),
                        Consolidation = messydates::as_messydate(Consolidation),
                        Acceptance = messydates::as_messydate(AccessionApprobation2)) %>%
  dplyr::distinct()

# Add titles and ID variables from HUGGO agreements data
agreements <- manytreaties::agreements$HUGGO %>%
  dplyr::filter(stringr::str_detect(Domain, "Environment")) %>%
  dplyr::select(c(gengID, Title, Begin, End, Signature, Force, ecolexID, ieaID))
MEA_edges <- dplyr::inner_join(MEA_edges, agreements, by = "gengID") %>%
  dplyr::distinct()

# Add treatyID column
MEA_edges$treatyID <- manypkgs::code_agreements(MEA_edges,
                                                MEA_edges$Title,
                                                MEA_edges$Begin)

# Join Data
HUGGO_MEM_env <- dplyr::full_join(HUGGO_MEM_env, MEA_edges) %>%
  dplyr::distinct()

# Add manyID column
manyID <- manypkgs::condense_agreements(idvar = HUGGO_MEM_env$treatyID)
HUGGO_MEM_env <- dplyr::left_join(HUGGO_MEM_env, manyID, by = "treatyID")

# Reorder variables
HUGGO_MEM_env <- dplyr::relocate(HUGGO_MEM_env, c("manyID", "treatyID",
                                                  "stateID", "Title", "Begin",
                                                  "End", "Signature",
                                                  "Force")) %>%
  dplyr::mutate(across(everything(), ~stringr::str_replace_all(., "^NA$",
                                                               NA_character_))) %>%
  dplyr::distinct() %>%
  plyr::ddply("manyID", zoo::na.locf, na.rm = FALSE) %>%
  dplyr::distinct() %>%
  dplyr::group_by(manyID) %>%
  tidyr::fill(.direction = "downup") %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::drop_na(Title) %>%
  tibble::as_tibble() %>%
  arrange(Begin) %>%
  mutate(Signature = messydates::as_messydate(Signature),
         Force = messydates::as_messydate(Force),
         Begin = messydates::as_messydate(Begin),
         End = messydates::as_messydate(End))

# Stage three: reconcile conflicts in data
# Step one: Match titles and dates of verified treaties from agreements$HUGGO
verified <- read.csv("data-raw/agreements/HUGGO/HUGGO_reconciled.csv")
# Step two: 
i <- 0
for(i in nrow(verified)){
  manyID <- verified[i, 1]
  treatyID <- verified[i, 13]
  title <- verified[i, 2]
  y <- 0
  y <- which(HUGGO_MEM_env$manyID == manyID & HUGGO_MEM_env$treatyID == treatyID &
                         HUGGO_MEM_env$Title == title)
  # Match dates
  # Begin
  HUGGO_MEM_env[y, "Begin"] <- messydates::as_messydate(verified[i, "Begin"])
  # End
  HUGGO_MEM_env[y, "End"] <- messydates::as_messydate(verified[i, "End"])
  # Signature
  HUGGO_MEM_env[y, "Signature"] <- messydates::as_messydate(verified[i, "Signature"])
  # Force
  HUGGO_MEM_env[y, "Force"] <- messydates::as_messydate(verified[i, "Force"])
  }

# Step two: Convert dates to mdate class
HUGGO_MEM_env$stateSignature <- messydates::as_messydate(HUGGO_MEM_env$stateSignature)
HUGGO_MEM_env$stateRat <- messydates::as_messydate(HUGGO_MEM_env$stateRat)
HUGGO_MEM_env$stateForce <- messydates::as_messydate(HUGGO_MEM_env$stateForce)
HUGGO_MEM_env$stateForce2 <- messydates::as_messydate(HUGGO_MEM_env$stateForce2)
HUGGO_MEM_env$stateForce3 <- messydates::as_messydate(HUGGO_MEM_env$stateForce3)
HUGGO_MEM_env$stateWithdrawal <- messydates::as_messydate(HUGGO_MEM_env$stateWithdrawal)
HUGGO_MEM_env$stateWithdrawal2 <- messydates::as_messydate(HUGGO_MEM_env$stateWithdrawal2)
HUGGO_MEM_env$Accession <- messydates::as_messydate(HUGGO_MEM_env$Accession)
HUGGO_MEM_env$Acceptance <- messydates::as_messydate(HUGGO_MEM_env$Acceptance)
HUGGO_MEM_env$ProvisionalApp <- messydates::as_messydate(HUGGO_MEM_env$ProvisionalApp)
HUGGO_MEM_env$Consent <- messydates::as_messydate(HUGGO_MEM_env$Consent)
HUGGO_MEM_env <- dplyr::distinct(HUGGO_MEM_env)

# Stage four: Merging verified data (HUGGO_MEM_env_reconciled.csv and HUGGO_MEM_env_additional.csv) with original data
HUGGO_MEM_env_reconciled <- readr::read_csv("data-raw/memberships/HUGGO_MEM/HUGGO_MEM_env_reconciled.csv") %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                End = messydates::as_messydate(End),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                stateSignature = messydates::as_messydate(stateSignature),
                stateRat = messydates::as_messydate(stateRat),
                stateForce = messydates::as_messydate(stateForce),
                stateForce2 = messydates::as_messydate(stateForce2),
                stateForce3 = messydates::as_messydate(as.character(stateForce3)),
                Term = messydates::as_messydate(Term),
                stateWithdrawal = messydates::as_messydate(stateWithdrawal),
                stateWithdrawal2 = messydates::as_messydate(as.character(stateWithdrawal2)),
                Deposit = messydates::as_messydate(Deposit),
                ProvisionalApp = messydates::as_messydate(ProvisionalApp),
                Reservation = messydates::as_messydate(Reservation),
                Consent = messydates::as_messydate(Consent),
                Acceptance = messydates::as_messydate(Acceptance),
                Accession = messydates::as_messydate(Accession),
                Succession = ifelse(!is.na(Succession), 1, 0))
HUGGO_MEM_env_additional <- readr::read_csv("data-raw/memberships/HUGGO_MEM/HUGGO_MEM_env_additional.csv") %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                End = messydates::as_messydate(End),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                stateSignature = messydates::as_messydate(stateSignature),
                stateRat = messydates::as_messydate(stateRat),
                stateForce = messydates::as_messydate(stateForce),
                stateForce2 = messydates::as_messydate(as.character(stateForce2)),
                stateForce3 = messydates::as_messydate(as.character(stateForce3)),
                Term = messydates::as_messydate(as.character(Term)),
                stateWithdrawal = messydates::as_messydate(stateWithdrawal),
                stateWithdrawal2 = messydates::as_messydate(as.character(stateWithdrawal2)),
                Deposit = messydates::as_messydate(as.character(Deposit)),
                ProvisionalApp = messydates::as_messydate(as.character(ProvisionalApp)),
                Reservation = messydates::as_messydate(as.character(Reservation)),
                Consent = messydates::as_messydate(as.character(Consent)),
                Acceptance = messydates::as_messydate(Acceptance),
                Accession = messydates::as_messydate(Accession),
                Succession = ifelse(!is.na(Succession), 1, 0))
HUGGO_MEM_env_new <- dplyr::bind_rows(HUGGO_MEM_env_reconciled, HUGGO_MEM_env_additional) %>%
  dplyr::arrange(Begin, manyID)

HUGGO_MEM_env <- dplyr::full_join(HUGGO_MEM_env, HUGGO_MEM_env_new,
                              by = c("manyID", "treatyID", "Title", "stateID"))
HUGGO_MEM_env <- HUGGO_MEM_env %>%
  manydata::transmutate(Begin = ifelse(!is.na(Begin.y), Begin.y, Begin.x),
                        End = ifelse(!is.na(End.y), End.y, End.x),
                        Signature = ifelse(!is.na(Signature.y), Signature.y,
                                           Signature.x),
                        Force = ifelse(!is.na(Force.y), Force.y, Force.x),
                        StateSignature = ifelse(!is.na(stateSignature.y),
                                                stateSignature.y,
                                                ifelse(stringr::str_detect(Verified_HUGGO, "stateSignature"),
                                                       stateSignature.y,
                                                       stateSignature.x)),
                        StateRatification = ifelse(!is.na(stateRat.y),
                                                   stateRat.y,
                                                   ifelse(stringr::str_detect(Verified_HUGGO, "stateRat"),
                                                          stateRat.y,
                                                          stateRat.x)),
                        StateForce = ifelse(!is.na(stateForce.y),
                                            stateForce.y,
                                            ifelse(stringr::str_detect(Verified_HUGGO, "stateForce"),
                                                   stateForce.y,
                                                   stateForce.x)),
                        StateForce2 = ifelse(!is.na(stateForce2.y),
                                             stateForce2.y, stateForce2.x),
                        StateForce3 = ifelse(!is.na(stateForce3.y),
                                             stateForce3.y, stateForce3.x),
                        StateEnd = ifelse(!is.na(stateWithdrawal.y),
                                          stateWithdrawal.y,
                                          ifelse(stringr::str_detect(Verified_HUGGO, "stateWithdrawal"),
                                                 stateWithdrawal.y,
                                                 stateWithdrawal.x)),
                        StateEnd2 = ifelse(!is.na(stateWithdrawal2.y),
                                          stateWithdrawal2.y, stateWithdrawal2.x),
                        Term = ifelse(!is.na(Term.y), Term.y, Term.x),
                        gengID = ifelse(!is.na(gengID.y), gengID.y, gengID.x),
                        ieaID = ifelse(!is.na(ieaID.y), ieaID.y, ieaID.x),
                        ecolexID = ifelse(!is.na(ecolexID.y), ecolexID.y, ecolexID.x),
                        Comments = ifelse(!is.na(Comments.y), Comments.y, Comments.x),
                        Deposit = ifelse(!is.na(Deposit.y), Deposit.y, Deposit.x),
                        obsolete = ifelse(!is.na(obsolete.y), obsolete.y, obsolete.x),
                        ProvisionalApp = ifelse(!is.na(ProvisionalApp.y),
                                                ProvisionalApp.y, ProvisionalApp.x),
                        Reservation = ifelse(!is.na(Reservation.y),
                                             Reservation.y, Reservation.x),
                        verified = ifelse(!is.na(verified.y), verified.y,
                                          verified.x),
                        Notes = ifelse(!is.na(Notes.y), Notes.y, Notes.x),
                        stateForce_ecolex = ifelse(!is.na(stateForce_ecolex.y),
                                                   stateForce_ecolex.y,
                                                   stateForce_ecolex.x),
                        stateForce_iea = ifelse(!is.na(stateForce_iea.y),
                                                stateForce_iea.y,
                                                stateForce_iea.x),
                        Consent = ifelse(!is.na(Consent.y),
                                         Consent.y,
                                         Consent.x),
                        Acceptance = ifelse(!is.na(Acceptance.y),
                                            Acceptance.y,
                                            Acceptance.x),
                        Accession = ifelse(!is.na(Accession.y),
                                           Accession.y,
                                           ifelse(stringr::str_detect(Verified_HUGGO, "Accession"),
                                                  Accession.y,
                                                  Accession.x))) %>%
  dplyr::relocate(manyID, treatyID, Title, Begin, stateID, Signature, Force, End,
                  StateSignature, StateRatification, StateForce, StateForce2,
                  StateForce3, StateEnd, StateEnd2,
                  gengID, ieaID, ecolexID, Accession, Succession) %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End),
                StateRatification = messydates::as_messydate(StateRatification),
                StateSignature = messydates::as_messydate(StateSignature),
                StateForce = messydates::as_messydate(StateForce),
                StateForce2 = messydates::as_messydate(StateForce2),
                StateForce3 = messydates::as_messydate(StateForce3),
                StateEnd = messydates::as_messydate(StateEnd),
                StateEnd2 = messydates::as_messydate(StateEnd2),
                stateForce_ecolex = messydates::as_messydate(stateForce_ecolex),
                stateForce_iea = messydates::as_messydate(stateForce_iea),
                Term = messydates::as_messydate(Term),
                Accession = messydates::as_messydate(Accession)) %>%
  dplyr::distinct() %>%
  # Remove variables used to track progress in coding
  dplyr::select(-c(Changes_HUGGO, Verified_HUGGO, Checked_HUGGO, verified))

# Mark successor states from Soviet Union, Yugoslavia, and Czechoslovakia in Succession variable
repstateIDs <- manypkgs::code_states(c("Armenia", "Azerbaijan", "Belarus",
                                       "Estonia", "Georgia", "Kazakhstan",
                                       "Kyrgyzstan", "Latvia", "Lithuania",
                                       "Moldova", "Russia", "Tajikistan",
                                       "Turkmenistan", "Ukraine", "Uzbekistan",
                                       "Serbia", "Bosnia and Herzegovina",
                                       "North Macedonia", "Slovenia",
                                       "Montenegro", "Croatia",
                                       "Czech Republic", "Slovakia"),
                                     activity = FALSE,
                                     replace = "ID")

HUGGO_MEM_env <- HUGGO_MEM_env %>%
  dplyr::mutate(Succession = ifelse(stateID %in% repstateIDs, 1, Succession))

HUGGO_MEM_env <- HUGGO_MEM_env %>%
  dplyr::mutate(Succession = ifelse(is.na(Succession), 0, Succession))

# Match non-English treaty titles to English treaty titles
noneng <- HUGGO_MEM_env %>%
  dplyr::filter(manyID == "UB08IB_1893A")
noneng <- noneng %>%
  dplyr::rename(match = manyID, Orig_noneng_title = Title) %>%
  # Non-English titles of the same agreement with an English title added in variable 'Orig_noneng_title',
  # manyID of the non-English agreement added in variable 'match'
  dplyr::arrange(match) %>%
  dplyr::mutate(manyID = "AH12IF_1893A")
noneng <- noneng %>%
  dplyr::select(manyID, Begin, Orig_noneng_title, match)
HUGGO_MEM_env <- dplyr::left_join(HUGGO_MEM_env, noneng, by = c("manyID", "Begin"))
# remove rows with non-English titles identified above
HUGGO_MEM_env <- HUGGO_MEM_env[-which(HUGGO_MEM_env$manyID == "UB08IB_1893A"),]

# Stage five: Format data correctly for export

# Correct StateSignature dates coded #### as missing
HUGGO_MEM_env <- HUGGO_MEM_env %>%
  dplyr::mutate(StateSignature = ifelse(grepl("######", StateSignature),
                                        NA, StateSignature),
                StateRatification = ifelse(grepl("######", StateRatification),
                                        NA, StateRatification))

# Make sure all data are in correct class
HUGGO_MEM_env <- HUGGO_MEM_env %>%
  dplyr::mutate(across(everything(),
                      ~stringr::str_replace_all(., "^NA$", NA_character_))) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End),
                StateRatification = messydates::as_messydate(StateRatification),
                StateSignature = messydates::as_messydate(StateSignature),
                StateForce = messydates::as_messydate(StateForce),
                StateForce2 = messydates::as_messydate(StateForce2),
                StateForce3 = messydates::as_messydate(StateForce3),
                StateEnd = messydates::as_messydate(StateEnd),
                StateEnd2 = messydates::as_messydate(StateEnd2),
                Term = messydates::as_messydate(Term),
                Accession = messydates::as_messydate(Accession))

### Trade agreements
# Stage one: Import verified data
HUGGO_MEM_trd <- readr::read_csv("data-raw/memberships/HUGGO_MEM/HUGGO_MEM_trade_additional.csv")

# Stage two: Correcting data

# Format data correctly
HUGGO_MEM_trd <- HUGGO_MEM_trd |>
  dplyr::mutate(Begin = dplyr::coalesce(Signature, Force)) |>
  dplyr::mutate(
    #standardise state names
    StateName2 = manypkgs::code_states(StateName, activity = FALSE,
                                       replace = "names"),
    StateName = ifelse(!is.na(StateName2) & StateName != StateName2, StateName2,
                       StateName),
    # check stateIDs are correct
    stateID = ifelse(is.na(stateID),
                     manypkgs::code_states(StateName, activity = FALSE,
                                           replace = "ID"),
                     stateID),
    stateID2 = manypkgs::code_states(StateName, activity = FALSE,
                                     replace = "ID"),
    stateID = ifelse(!is.na(stateID2) & stateID != stateID2, stateID2, stateID),
    across(everything(),
           ~stringr::str_replace_all(., "^NA$", NA_character_))) |>
  dplyr::distinct() |>
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End),
                StateRatification = messydates::as_messydate(StateRatification),
                StateSignature = messydates::as_messydate(StateSignature),
                StateForce = messydates::as_messydate(StateForce),
                StateEnd = messydates::as_messydate(StateEnd),
                Accession = messydates::as_messydate(Accession)) |>
  dplyr::select(-c(StateName2, stateID2)) |>
  dplyr::arrange(Begin)

# correct StateName for Hong Kong
HUGGO_MEM_trd <- HUGGO_MEM_trd |>
  dplyr::mutate(StateName = ifelse(StateName == "Hong Kong",
                                   "China - Hong Kong",
                                   StateName))

# Add manyIDs and treatyIDs for newly added entries
HUGGO_MEM_trd$treatyID <- manypkgs::code_agreements(HUGGO_MEM_trd, HUGGO_MEM_trd$Title,
                                                HUGGO_MEM_trd$Begin)

manyID <- manypkgs::condense_agreements(manytreaties::memberships)
HUGGO_MEM_trd <- dplyr::left_join(HUGGO_MEM_trd, manyID, by = "treatyID") |>
  dplyr::distinct()
HUGGO_MEM_trd <- HUGGO_MEM_trd |>
  dplyr::mutate(manyID = ifelse(!is.na(manyID.x), manyID.x, manyID.y)) |>
  dplyr::select(-c(manyID.x, manyID.y)) |>
  dplyr::relocate(manyID, treatyID)

### Health agreements
# Stage one: Import verified data
HUGGO_MEM_hlt <- readr::read_csv("data-raw/memberships/HUGGO_MEM/HUGGO_MEM_health_additional.csv")

# Stage two: Correcting data
HUGGO_MEM_hlt <- as_tibble(HUGGO_MEM_hlt) %>%
  dplyr::mutate(stateID = manypkgs::code_states(StateName, activity = FALSE,
                                                replace = "ID"),
                Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End),
                StateSignature = messydates::as_messydate(StateSignature),
                StateRatification = messydates::as_messydate(StateRatification),
                StateEnd = messydates::as_messydate(StateEnd),
                StateForce = messydates::as_messydate(StateForce)) %>%
  dplyr::arrange(Begin)

# Code state names correctly
HUGGO_MEM_hlt <- HUGGO_MEM_hlt %>%
  dplyr::mutate(StateName = ifelse(StateName == "Democratic Republic of the Congo - Congo",
                                   "Democratic Republic of the Congo",
                                   StateName),
                # correct double StateNames and stateIDs
                stateID = ifelse(stateID == "COD - COG",
                                 "COD",
                                 stateID)) %>%
  # remove entries for NA: StateNames that were European Atomic Energy Community
  # and World Psychiatric Association
  dplyr::filter(!is.na(StateName))

# Ensure NAs and data are coded correctly
HUGGO_MEM_hlt <- HUGGO_MEM_hlt %>%
  dplyr::mutate(Begin = ifelse(Begin == "-", NA, Begin),
                Signature = ifelse(Signature == "-", NA, Signature),
                Force = ifelse(Force == "-", NA, Force),
                End = ifelse(End == "-", NA, End),
                StateSignature = ifelse(StateSignature == "-", NA,
                                        StateSignature),
                StateRatification = ifelse(StateRatification == "-", NA,
                                           StateRatification),
                StateForce = ifelse(StateForce == "-", NA, StateForce),
                StateEnd = ifelse(StateEnd == "-", NA, StateEnd)) %>%
  dplyr::mutate(across(everything(),
                       ~stringr::str_replace_all(., "^NA$", NA_character_))) %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End),
                StateSignature = messydates::as_messydate(StateSignature),
                StateRatification = messydates::as_messydate(StateRatification),
                StateForce = messydates::as_messydate(StateForce),
                StateEnd = messydates::as_messydate(StateEnd),
                StateName = manypkgs::standardise_titles(StateName),
                Accession = messydates::as_messydate(Accession)) %>%
  dplyr::arrange(Begin, manyID, stateID) %>%
  dplyr::select(manyID, Title, Begin, stateID, StateName,
                StateSignature, StateRatification, StateForce, StateEnd,
                `Rat=Notif`, Accession, Succession,
                treatyID, Signature, Force, End, Coder) %>%
  dplyr::distinct()

### Merge data from different domains into one dataset
HUGGO_MEM_env$Domain <- "Environment"
HUGGO_MEM_trd$Domain <- "Trade"
HUGGO_MEM_hlt$Domain <- "Health"
overlap <- HUGGO_MEM_hlt %>%
  dplyr::filter(manyID == "CTMHWD_1989A" | manyID == "NCLRSF_1994A" |
                  manyID == "FRMWTC_2003A")
HUGGO_MEM_hlt_rem <- HUGGO_MEM_hlt %>%
  dplyr::filter(manyID != "CTMHWD_1989A" & manyID != "NCLRSF_1994A" &
                  manyID != "FRMWTC_2003A")

HUGGO_MEM <- dplyr::bind_rows(HUGGO_MEM_env, HUGGO_MEM_trd, HUGGO_MEM_hlt_rem)

# Add in overlapping agreements
HUGGO_MEM <- dplyr::full_join(HUGGO_MEM, overlap,
                              by = c("manyID", "treatyID", "stateID", "Begin"))

HUGGO_MEM <- HUGGO_MEM %>%
  dplyr::mutate(StateSignature = ifelse(!is.na(Domain.y), StateSignature.y,
                                        StateSignature.x),
                StateRatification = ifelse(!is.na(Domain.y), StateRatification.y,
                                           StateRatification.x),
                StateForce = ifelse(!is.na(Domain.y), StateForce.y,
                                    StateForce.x),
                StateEnd = ifelse(!is.na(Domain.y), StateEnd.y,
                                  StateEnd.x),
                Accession = ifelse(!is.na(Domain.y), Accession.y,
                                   Accession.x),
                Succession = ifelse(!is.na(Domain.y), Succession.y,
                                    Succession.x),
                Signature = ifelse(!is.na(Domain.y), Signature.y,
                                   Signature.x),
                Force = ifelse(!is.na(Domain.y), Force.y,
                               Force.x),
                End = ifelse(!is.na(Domain.y), End.y,
                             End.x),
                Coder = ifelse(!is.na(Coder.x) & !is.na(Coder.y),
                               "Diego, Jael",
                               ifelse(is.na(Coder.x), Coder.y, Coder.x)),
                Domain = ifelse(!is.na(Domain.x) & !is.na(Domain.y),
                               "Environment, Health",
                               ifelse(is.na(Domain.x), Domain.y, Domain.x)),
                `Rat=Notif` = ifelse(!is.na(`Rat=Notif.x`), `Rat=Notif.x`,
                                     `Rat=Notif.y`),
                StateName = ifelse(!is.na(StateName.x), StateName.x,
                                   StateName.y)) %>%
  dplyr::rename(Title = Title.x, Title2 = Title.y)

# Remove unnecessary variables from merging
HUGGO_MEM <- HUGGO_MEM %>%
  dplyr::select(-c(StateSignature.x, StateSignature.y, StateRatification.x,
                   StateRatification.y, StateForce.x, StateForce.y,
                   StateEnd.x, StateEnd.y, Accession.x, Accession.y,
                   Succession.x, Succession.y, Signature.x, Signature.y,
                   Force.x, Force.y, End.x, End.y, Coder.x, Coder.y,
                   Domain.x, Domain.y, `Rat=Notif.x`, `Rat=Notif.y`,
                   StateName.x, StateName.y)) %>%
  dplyr::distinct()

# Replace NA titles from joining overlapping agreements manually
HUGGO_MEM <- HUGGO_MEM %>%
  dplyr::mutate(Title = ifelse(is.na(Title) & manyID == "FRMWTC_2003A",
                               "WHO Framework Convention On Tobacco Control (FCTC)",
                               ifelse(is.na(Title) & manyID == "CTMHWD_1989A",
                                      "Convention On The Control Of Transboundary Movements Of Hazardous Wastes And Their Disposal",
                                      ifelse(is.na(Title) & manyID == "NCLRSF_1994A",
                                             "Convention On Nuclear Safety",
                                             Title))))

### Reorder variables for clarity
HUGGO_MEM <- HUGGO_MEM %>%
  dplyr::relocate(manyID, Title, Begin, stateID, StateName, StateSignature,
                  StateRatification, StateForce, StateEnd, `Rat=Notif`,
                  Accession, Succession, treatyID, Signature, Force, End,
                  gengID, url) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Begin, manyID, stateID) %>%
  dplyr::relocate(c(Domain, Coder), .after = last_col())

# Ensure dates are coded correctly
HUGGO_MEM <- HUGGO_MEM %>%
  dplyr::mutate(across(everything(),
                       ~stringr::str_replace_all(., "^NA$", NA_character_))) %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End),
                StateSignature = messydates::as_messydate(StateSignature),
                StateRatification = messydates::as_messydate(StateRatification),
                StateForce = messydates::as_messydate(StateForce),
                StateEnd = messydates::as_messydate(StateEnd),
                StateName = manypkgs::standardise_titles(StateName),
                Accession = messydates::as_messydate(Accession),
                StateForce2 = messydates::as_messydate(StateForce2),
                StateForce3 = messydates::as_messydate(StateForce3),
                StateEnd2 = messydates::as_messydate(StateEnd2),
                stateForce_ecolex = messydates::as_messydate(stateForce_ecolex),
                stateForce_iea = messydates::as_messydate(stateForce_iea),
                Term = messydates::as_messydate(Term))

# Rename dataset to avoid clashes when consolidating
HUGGO <- HUGGO_MEM

# Remove manyID, use treatyID for identifying observations and relating datasets
HUGGO <- HUGGO %>%
  dplyr::select(-manyID) %>%
  dplyr::relocate(treatyID)

# Stage three: Connecting data
# Next run the following line to make HUGGO available
# within the package.
manypkgs::export_data(HUGGO, datacube = "parties",
                      URL = "Hand-coded by the GGO team")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure
# adherence to certain standards. You can hit Cmd-Shift-T (Mac)
# or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill
# in as much detail about the variables etc as possible.
