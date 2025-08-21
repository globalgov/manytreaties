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
                        Location = `Place IEA was concluded`,
                        TextURL = `URL to text of IEA`,
                        Language = tolower(gsub("\\..*", "", Lang1)),
                        Subject = `Issue area (subject)`,
                        Term = messydates::as_messydate(`Term Date`),
                        Grounds = dplyr::coalesce(`Term Type`,
                                                  ifelse(is.na(`Term by ID`), 
                                                         NA_character_, 
                                                         paste("Term by", 
                                                               `Term by ID`))),
                        TitleAlt = `Alternative Treaty Name`,
                        Auspices = `Org Auspices`,
                        Coder = `Researcher`,
                        Comments = `Data entry notes`) %>%
  dplyr::mutate(Signature = dplyr::case_when(Signature == "1111-11-11" ~ NA,
                                             .default = Signature),
                Force = dplyr::case_when(Force == "1111-11-11" ~ NA,
                                         .default = Force),
                Term = dplyr::case_when(Term == "1111-11-11" ~ NA,
                                        .default = Term)) %>% 
  dplyr::mutate(Begin = messydates::as_messydate(dplyr::coalesce(Signature, Force)),
                End = Term) %>%
  dplyr::select(-dplyr::contains("(legacy)")) %>%
  manydata::transmutate(TypeAgree = dplyr::case_match(`Agreement Type Level 3`,
                                                          c("Agreement","Acuerdo") ~ "Agreement",
                                                          c("Convention","Convenzione","Convencion","Convenio") ~ "Convention",
                                                          c("Declaration","Declaracion") ~ "Declaration",
                                                          c("Treaty","Tratado") ~ "Treaty",
                                                          c("Protocol","Protocolo","Protocole") ~ "Protocol",
                                                          .default = `Agreement Type Level 3`),
                        TypeAmbit = dplyr::case_match(`Inclusion (type of agreements)`, 
                                                  c("BEA","Bilateral (2 and only 2 governments)") ~ "Bilateral", 
                                                  c("MEA","Multilateral (3 or more governments)") ~ "Multilateral",
                                                  .default = `Inclusion (type of agreements)`)) %>%
  dplyr::select(-c(AmendProtToA,UNEP2005pg,`URL to membership list`,
                   `Date *Bilateral* signed by 2nd state (ONLY if BEA)`, UNTS,
                   `E (environment) Code (agreement is environmental or not)`,
                   `Possible Sources (additional)`,`Treaty Text`,`Data complete`,
                   `Membership eligibility (multilateral, bilateral, etc)`,
                   `Words used to code as Environmental`, `Sequence in lineage`,
                   Category, `Coded Text`, `Source for E (environmental) code`,
                   `Text used for E (environmental) coding`, Nid))

# Add treatyID column
IEADB <- IEADB %>% 
  dplyr::mutate(treatyID = manytreaties::code_agreements(IEADB, IEADB$Title, 
                                                         IEADB$Begin))
IEADB <- IEADB %>% 
  dplyr::select(treatyID, Title, Begin, End, Signature, Force, 
                Term, Grounds,
                TypeAmbit, TypeAgree, 
                Lineage, Subject,
                Location, Auspices, Secretariat, TitleAlt, 
                TextURL, Language,
                Coder, Comments, Version, mitchID,
                everything()) %>% 
  dplyr::filter(TypeAmbit == "Multilateral" |
                  TypeAmbit == "Bilateral") %>%
  dplyr::distinct(treatyID, .keep_all = TRUE) %>%
  dplyr::arrange(Begin)
glimpse(IEADB)

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
