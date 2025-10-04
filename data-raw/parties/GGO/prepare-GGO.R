# GGO Parties Preparation Script

GGO <- readr::read_csv("data-raw/parties/GGO/HUGGO_parties_clean.csv") %>% 
  dplyr::select(-c(StateForce3,Accession,ProvisionalApp,Reservation,Pathway))
GGO2 <- GGO %>% dplyr::filter(!is.na(StateForce2)) %>% 
  transmutate(StateForce = messydates::as_messydate(StateForce2),
              StateEnd = messydates::as_messydate(StateEnd2))
GGO <- GGO %>% dplyr::select(-c(StateForce2,StateEnd2)) %>% 
  dplyr::mutate(StateForce = messydates::as_messydate(StateForce),
                StateEnd = messydates::as_messydate(StateEnd))
GGO <- dplyr::bind_rows(GGO, GGO2) %>% 
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End),
                StateSignature = messydates::as_messydate(StateSignature),
                StateRatification = messydates::as_messydate(StateRatification))
GGO <- GGO %>% 
  dplyr::mutate(treatyID = manytreaties::code_agreements(GGO, GGO$Title, GGO$Begin))

# Stage three: Connecting data
manypkgs::export_data(GGO, datacube = "parties",
                      URL = "www.panarchic.ch")
