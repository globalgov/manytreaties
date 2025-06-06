# DESTA Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many package.
library(manypkgs)

# Stage one: Collecting data
# Note that the original data (in excel format) has been converted and saved as
# a csv file with the same variables and data.
DESTA <- read.csv2("data-raw/agreements/DESTA/DESTA.csv")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'DESTA' object until the object created
# below (in stage three) passes all the tests.
DESTA <- tibble::as_tibble(DESTA) %>%
  dplyr::filter(typememb != "5", typememb != "6",  typememb != "7",
                entry_type != "accession", entry_type != "withdrawal") %>%
  #categories removed because they relate to changes in membership that are
  #reflected in the memberships datacube
  dplyr::rename(Ambit = typememb, AgreementType = entry_type) %>%
  dplyr::case_match(Ambit,
                    "1" ~ "B",
                    "2" ~ "P",
                    "3" ~ "P-3",
                    "4" ~ "P-R") %>%
  # P-3 distinguishes plurilateral agreements with third countries.
  # P-R distinguishes plurilateral agreements between regions
  dplyr::case_match(AgreementType,
                    "base_treaty" ~ "A",
                    "protocol or amendment" ~ "P/E",
                    "consolidated" ~ "P/E",
                    "negotiation" ~ "A") %>%
  dplyr::rename(GeogArea = regioncon) %>%
  dplyr::case_match(GeogArea,
                    "Intercontinental" ~ "G",
                    "Asia" ~ "R",
                    "Africa" ~ "R",
                    "Americas" ~ "R",
                    "Europe" ~ "R",
                    "Oceania" ~ "R") %>%
  manydata::transmutate(destaID = as.character(`base_treaty`),
                        Title = manypkgs::standardise_titles(name)) %>%
  dplyr::mutate(beg = dplyr::coalesce(year, entryforceyear)) %>%
  dplyr::arrange(beg) %>%
  # standardise date formats across agreements datacube
  dplyr::mutate(beg = ifelse(beg == "NA", "NA", paste0(beg, "-01-01"))) %>%
  dplyr::mutate(year = ifelse(year == "NA", "NA", paste0(year, "-01-01"))) %>%
  dplyr::mutate(entryforceyear = ifelse(entryforceyear == "NA",
                                        "NA",
                                        paste0(entryforceyear, "-01-01"))) %>%
  manydata::transmutate(Begin = messydates::as_messydate(as.character(beg)),
                        Signature = messydates::as_messydate(as.character(year)),
                        Force = messydates::as_messydate(as.character(entryforceyear))) %>%
  dplyr::select(destaID, Title, Begin, Signature, Force, AgreementType, Ambit,
                GeogArea)

# Add treatyID column
DESTA$treatyID <- manypkgs::code_agreements(DESTA, DESTA$Title, DESTA$Begin)

# Add manyID column
# manyID <- manypkgs::condense_agreements(manytrade::agreements)
# DESTA <- dplyr::left_join(DESTA, manyID, by = "treatyID")

# Re-order the columns
DESTA <- DESTA %>% 
  dplyr::select(treatyID, Title, Begin, AgreementType, Ambit, GeogArea,
                Signature, Force, destaID) %>% 
  dplyr::arrange(Begin)

# manypkgs includes several functions that should help cleaning and 
# standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make DESTA available within the many universe.
manypkgs::export_data(DESTA, datacube = "agreements",
                      URL = "https://www.designoftradeagreements.org/downloads/")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to
# certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests
# locally at any point.
# Any test failures should be pretty self-explanatory and may require you to
# return to stage two and further clean, standardise, or wrangle your data into
# the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please note that the export_data() function requires a .bib file to be
# present in the data_raw folder of the package for citation purposes.
# Therefore, please make sure that you have permission to use the dataset
# that you're including in the package.
# Please make sure that you cite any sources appropriately and fill in as much
# detail about the variables etc as possible.
