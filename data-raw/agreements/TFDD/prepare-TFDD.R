# TFDD Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many package.

# Stage one: Collecting data
TFDD <- readr::read_csv("data-raw/agreements/TFDD/TFDD.csv")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'TFDD' object until the object created
# below (in stage three) passes all the tests.
# We recommend that you avoid using one letter variable names to keep
# away from issues with ambiguous names down the road.
TFDD <- as_tibble(TFDD) %>%
  dplyr::mutate(Signature = messydates::as_messydate(DateSigned),
                Begin = messydates::as_messydate(as.character(Signature))) %>%
  manydata::transmutate(tfddID = `2016Update ID`,
                        Title = manypkgs::standardise_titles(DocumentName),
                        Lineage = PrimaryTFDDID, # original agreement to which a replacement, amendment or protocol refers
                        Basin = `Basin Name`,
                        Issue = `Issue Area`) %>%
  dplyr::mutate(AgreementType = dplyr::case_match(DocType,
                  c(2, 4, 8) ~ "A",
                  6 ~ "E",
                  5 ~ "S", # Superseded
                  7 ~ "P",
                  .default = NA),
                NotTreaty = ifelse(DocType == 1, 1, 0),
                Water_as_consumable_rsc = ifelse(DocType == 3, 0, 1),
                GeogArea = dplyr::case_match(GeoScope,
                  1 ~ "G",
                  2 ~ "R",
                  3 ~ "SR", # Subregional
                  4 ~ "B", # Basin
                  5 ~ "SB", # Sub-basin
                  .default = NA),
                DocType = dplyr::case_match(NumberParties,
                  1 ~ "B",
                  2 ~ "M"
                )) %>%
  dplyr::select(Title, Begin, Signature, AgreementType, DocType, GeogArea,
                Water_as_consumable_rsc, NotTreaty, Issue, Basin,
                Lineage, tfddID) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Begin)

# Add a treatyID column
TFDD$treatyID <- manypkgs::code_agreements(TFDD,
                                           TFDD$Title,
                                           TFDD$Begin)

# Add manyID column
manyID <- manypkgs::condense_agreements(manytreaties::agreements)
TFDD <- dplyr::left_join(TFDD, manyID, by = "treatyID")

# Re-order the columns
TFDD <- dplyr::relocate(TFDD, manyID)

# Remove duplicates and ensure NAs are coded correctly
TFDD <- TFDD %>%
  dplyr::filter(!is.na(Title)) %>%
  dplyr::mutate(manyID = ifelse(is.na(manyID), treatyID, manyID),
                Issue = dplyr::case_match(Issue,
                                          c("0", "#N/A", "Y") ~ NA,
                                          .default = Issue),
                across(everything(),
                       ~stringr::str_replace_all(.,
                                                 "^NA$", NA_character_))) %>%
  dplyr::mutate(Begin = messydates::as_messydate(Begin),
                Signature = messydates::as_messydate(Signature)) %>%
  dplyr::distinct(.keep_all = TRUE)

# manypkgs includes several functions that should help with
# cleaning and standardising your data
# such as `standardise_titles()` and `standardise_texts()`.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make TFDD available
# within the package.
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence
# to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows)
# to run these tests locally at any point.
# Any test failures should be pretty self-explanatory
# and may require you to return to stage two and further clean,
# standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please note that the export_data() function requires a .bib file to be
# present in the data_raw folder of the package for citation purposes.
# Therefore, please make sure that you have permission to use the dataset
# that you're including in the package.
# To add a template of .bib file to the package,
# please run `manypkgs::add_bib("agreements", "TFDD")`.
manypkgs::export_data(TFDD, datacube = "agreements",
                      URL = "https://transboundarywaters.science.oregonstate.edu/")
