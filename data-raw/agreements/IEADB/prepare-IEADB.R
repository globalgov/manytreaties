# IEADB Preparation Script

# This is a template for importing, cleaning, and exporting data
# for the 'many' packages.

# Stage one: Collecting data
IEADB <- readr::read_delim("data-raw/agreements/IEADB/db_treaties.csv", ",")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IEADB' object until the object created
# below (in stage three) passes all the tests.
IEADB <- as_tibble(IEADB)  %>%
  manydata::transmutate(mitchID = as.character(`IEA#`),
                        Title = manytreaties::standardise_titles(`Agreement Name`),
                        Signature = messydates::as_messydate(`Date IEA was concluded`),
                        Force = messydates::as_messydate(`Date IEA entered into force`),
                        TextURL = `URL to text of IEA`,
                        Language = Lang1,
                        PartyURL = `URL to membership list`,
                        Sequence = `Sequence in lineage`,
                        Subject = `Issue area (subject)`,
                        Term = messydates::as_messydate(`Term Date`),
                        Grounds = dplyr::coalesce(`Term Type`,
                                                  paste("Term by", `Term by ID`)),
                        OrigTitle = `Alternative Treaty Name`,
                        Auspices = `Org Auspices`,
                        AdoptedIn = `Place IEA was concluded`,
                        Comments = `Data entry notes`,
                        Coded = `Data complete`,
                        Coder = `Researcher`) %>%
  dplyr::mutate(Begin = messydates::as_messydate(dplyr::coalesce(Signature, Force)),
                End = Term) %>%
  dplyr::select(-dplyr::contains("(legacy)")) %>%

IEADB <- IEADB %>% 
  # Add treatyID column
  dplyr::mutate(treatyID = manytreaties::code_agreements(IEADB, IEADB$Title, 
                                                         IEADB$Begin))
IEADB <- IEADB %>% 
  dplyr::select(treatyID, Title, Begin, End, Signature, Force, Term, 
                Ambit, AgreementType, 
                Lineage, AdoptedIn, Auspices, Secretariat, OrigTitle, 
                TextURL, 
                Comments, Coder, everything()) %>% 
  
  dplyr::filter(Ambit == "Multilateral" |
                  Ambit == "Bilateral") %>%
  dplyr::distinct(treatyID, .keep_all = TRUE) %>%
  dplyr::arrange(Begin)
IEADB

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IEADB available
# within the package.
manypkgs::export_data(IEADB, datacube = "agreements",
                      URL = "https://www.iea.ulaval.ca/en")
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
