# TFDD Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many package.

# Stage one: Collecting data
TFDD <- readr::read_csv("data-raw/agreements/TFDD/TFDD.csv",
                        na = c("", "NA", "N/A", "#N/A","N.A."))

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'TFDD' object until the object created
# below (in stage three) passes all the tests.
# We recommend that you avoid using one letter variable names to keep
# away from issues with ambiguous names down the road.
TFDD <- as_tibble(TFDD) %>%
  dplyr::select(-matches("2010|_Article|_Text|TFDDID|Preced|Construct_")) %>%
  dplyr::select(-c(`Entry ID`, BCCODE, BCODE, CCODE, Country_Name, Signatories,
                   `Unified Primary 2016 Treaty ID`,`DocID Linkage`,RBO,
                   Region_Continent,`Treaty Basin(s)`,`TFDD Basin(s)`,MinorAgreement,
                   Allocation,Environment,Format,NeedsVariability,AltScenarios,
                   `Conflict Resolution Mechanism`,NonSig_Riparians,VarMgmt,
                   StakeholderParticip,Compensation,`Uncertainty Explicit`,
                   PriorNotification,`NonWater Linkage`,EnforceMech,Enforcement,
                   WaterDiv_Method,`Agreement Purpose`,GeoScope)) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DateSigned),
                        Begin = messydates::as_messydate(as.character(Signature)),
                        tfddID = as.character(`2016Update ID`),
                        Title = manytreaties::standardise_titles(DocumentName),
                        # TypeAmbit = dplyr::case_match(GeoScope,
                        #                               1 ~ "Global",
                        #                               2 ~ "Regional",
                        #                               3 ~ "Subregional", # Subregional
                        #                               4 ~ "Basin", # Basin
                        #                               5 ~ "Subbasin", # Sub-basin
                        #                               .default = NA),
                        TypeAgree = dplyr::case_match(DocType,
                                                      c(2, 4, 8) ~ "Agreement",
                                                      6 ~ "Amendment",
                                                      # 5 ~ "S", # Superseded
                                                      7 ~ "Protocol",
                                                      .default = NA),
                        TypeAmbit = dplyr::case_match(NumberParties,
                                                      1 ~ "Bilateral",
                                                      2 ~ "Multilateral"
                        ),
                        # Lineage = PrimaryTFDDID, # original agreement to which a replacement, amendment or protocol refers
                        TypeRegion = PRIMARY_COUNTRY_SUBREGION,
                        TypeGeo = dplyr::coalesce(`Basin Name`,Basin_Name),
                        TypeSubject = `Issue Area`,
                        Secretariat = CommName,
                        Location = LocationSigned,
                        Comments = dplyr::coalesce(Notes, AdditionalComments,NonWaterLink_Comments)) %>%
  dplyr::select(Title, Begin, Signature, 
                TypeAgree, TypeAmbit, TypeRegion, TypeGeo, TypeSubject,
                Location, Secretariat, Comments, Source, tfddID, everything()) %>%
  dplyr::filter(!is.na(Title)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Begin)

# Add a treatyID column
TFDD$treatyID <- manytreaties::code_agreements(TFDD,
                                           TFDD$Title,
                                           TFDD$Begin)

# Add manyID column
# manyID <- manypkgs::condense_agreements(manytreaties::agreements)
# TFDD <- dplyr::left_join(TFDD, manyID, by = "treatyID")

# Re-order the columns
TFDD <- dplyr::relocate(TFDD, treatyID)

# Remove duplicates and ensure NAs are coded correctly
# TFDD <- TFDD %>%
#   dplyr::mutate(Begin = messydates::as_messydate(Begin),
#                 Signature = messydates::as_messydate(Signature)) %>%
#   dplyr::distinct(.keep_all = TRUE)

# Stage three: Connecting data
manypkgs::export_data(TFDD, datacube = "agreements",
                      URL = "https://transboundarywaters.science.oregonstate.edu/")
