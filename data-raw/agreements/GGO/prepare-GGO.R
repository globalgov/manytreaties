# GGO Preparation Script

# Import HUGGO_clean ####
HUGGO <- readr::read_csv("data-raw/agreements/GGO/HUGGO_clean.csv")
# code_extend(HUGGO$Title, HUGGO$TypeAmbit)
# code_extend(HUGGO$Title, HUGGO$TypeAgree)
# code_extend(HUGGO$Title, HUGGO$Region)
# code_extend(HUGGO$Title, HUGGO$Subject)

HUGGO$treatyID <- manytreaties::code_agreements(HUGGO)
GGO <- HUGGO %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                End = messydates::as_messydate(End),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                Term = messydates::as_messydate(Term)) %>%
  dplyr::select(treatyID, everything()) %>%
  dplyr::arrange(Begin)

# Connecting data ####
manypkgs::export_data(GGO, datacube = "agreements",
                      URL = "https://panarchic.ch")
