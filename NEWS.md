# manytreaties 1.0.0

## Package

- Updated README
- Updated website

## Data

- Updated GGO agreements (closes #34)
  - Fixed various inconsistent dates (thanks @myevrard, @jaeltan)
  - Fixed duplicates (thanks @myevrard)
  - Substantially more coding on TypeRegion and TypeSubject (closes #15)
  - Standardised TypeSubject coding to be more consistent
  - TypeAmbit now codes not just "Bilateral" and "Multilateral",
  but also "Regional" (plurilateral), "Trilateral", "Interministerial", and
  "Interregional"
  - TypeRegion now codes main region instead of multiregion
  - Removed outdated variables (e.g. GENGID)
  - Using "None" for confirmed none now
  - Added codebook
  - Fixed citations
- Updated GGO parties
  - Fixed various inconsistent dates (thanks @myevrard)
  - Fixed duplicates (thanks @myevrard)
  - Added codebook
  - Fixed citations
- Updated IEADB agreements (closes #35)
- Updated IEADB parties (closes #36)
- Updated TFDD preparation
- Updated DESTA preparation and reexported

## Functions

- Added `standardise_*()` and `code_*()` functions from `{manypkgs}`
- Improved `standardise_treaty_text()`
  - Uses `{stringi}` now for speed
- Improved `standardise_titles()`
  - Dropped "The Government Of" from treaty titles
  - Gave function more internal structure
  - Uses `{stringi}` and own stringi-based `mgsub()` (internal) now for speed
  - Uses internal number words for speed
- Added `standardise_caps()` as separate function for capitalising every word,
  not the same as title case
- Improved `code_agreements()`
  - Now uses `{cli}`
  - Uses `{stringi}` now for speed
- Improved `code_acronyms()`
  - Renamed from `code_acronym()` to be more consistent
  - Uses `{stringi}` now for speed
  - Now removes all cities from central list
  - Alphabetised abbreviations list
- Improved `code_term_date()`
  - Uses `{stringi}` now for speed
- Improved `condense_agreements()`
  - Uses `{stringi}` now for speed
- Added `code_area()` for coding the geographical area

# manytreaties 0.1.1

2025-06-03

## Package
* Updated healthdata vignette to match changes to datasets
* Updated pkgdown file with new logo
* Updated documentation for `agreements` and `parties` datacubes
* Closed #12 by moving over some functions from `{manypkgs}`

## Data
* Added back DESTA in `agreements` and `parties` datacube
* Corrected some errors in dates in `HUGGO` dataset
* Added AgreementType and Ambit data for trade agreements in `HUGGO` dataset
* Removed manyID in favour of using treatyID as identifier in datasets
* Closed #17 and #21 by updating treaty texts files

# manytreaties 0.1.0

2025-02-27

## Package
* Closed #2 by updating GitHub actions in prchecks and pushrelease

## Data
* Consolidated variables in `agreements$HUGGO`
  * Closed #4 by removing `verified` and `Download` variables
  * Removed `Sequence`, `Formal`, and `Appendices` variables as not currently necessary
  * Combined `DocType` and `Ambit` variables into `Ambit`
  * Combined `Source`, `Coded`, and `Notes` variables into `Comments`
  * Combined `Domain` and `Topic` variables together into `Domain`
  * Combined `AgreementType` and `MEA_type` variables together into `AgreementType`
  * Used abstract links in `TextURL` where URL is not available
* Corrected errors to data in HUGGO
* Dropped GHHR and WHO datasets from `agreements`
* Added TFDD dataset in `agreements`
* Reordered datasets in `agreements` datacube by number of observations
* Renamed DocType variable Ambit for all other datasets in agreements
* Added license badges to `agreements` and `parties` datacubes
* Renamed `memberships` datacube `parties`
* Added recent UK trade agreements to `parties$HUGGO`
* Improved variable mapping explanation
* Improved dataset documentation by making it computational, updating links and sources, and tabulated variable mapping

# manytreaties 0.0.1

2024-10-08

## Package

* Set up manytreaties package using manydata::setup_package()
  * Added `DESCRIPTION` file
  * Added `R` folder
  * Added `LICENSE` file
  * Added `NAMESPACE` file
  * Added `NEWS` file
  * Added `README` file
  * Added `.github` folder
  * Added `CODE_OF_CONDUCT` file
  * Added `CONTRIBUTING` file
  * Added `pull_request_template` file
  * Added `ISSUE_TEMPLATE` folder
  * Added `bug_report` file
  * Added `feature_request` file
  * Added `workflows` folder
  * Added `prchecks` file
  * Added `pushrelease` file
  * Added `prcommands` file
  * Added `tests` folder
  * Added `testthat` folder
  * Added `testthat` file
* Added package logo
* Added package website

## Data

* Added `agreements` and `memberships` datacube
  * Added `IEADB`, `IEADB_MEM`, `HEIDI`, `TFDD_MEM` datasets for international environmental agreements.
  * Added `GPTAD`, `LABPTA`, `TOTA`, `TREND`, `GPTAD_MEM` datasets for international trade agreements.
  * Added `WHO`, `GHHR` datasets for international health agreements.
  * Added merged `HUGGO` dataset with handcoded data on environmental, trade, and health agreements.
* Added tests for all datasets
