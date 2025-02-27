# manytreaties 0.1.0

2025-02-27

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
* Added license badges to `agreements` and `parties` datacubes
* Renamed `memberships` datacube `parties`
* Added recent UK trade agreements to `parties$HUGGO`
* Improved variable mapping explanation
* Improved dataset documentation by making it computational, updating links and sources

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
