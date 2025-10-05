#' Code Treaty Termination Type
#'
#' Creates a column with the type of termination
#' of the treaty.
#' @param title title column variable
#' @param text treaty text column variable
#' @return a character vector with the termination clause type
#' of the treaty
#' @examples
#' \dontrun{
#' m <- manyenviron::agreements$HUGGO[200:300,]
#' code_term(m$Title)
#' code_term(m$Title, m$Text)
#' }
#' @export
code_term <- function(title, text = NULL) {
  if (missing(title)) {
    # If missing argument, function returns list of termination clause types
    type <- as_tibble(termination_type)
    type$meaning[7] <- paste(substr(type$meaning[7], 0, 100), "...")
    type$meaning[10] <- paste(substr(type$meaning[10], 0, 100), "...")
    type$meaning[11] <- paste(substr(type$meaning[11], 0, 100), "...")
    type$meaning[12] <- paste(substr(type$meaning[12], 0, 100), "...")
    type <- knitr::kable(type, "simple",
                         caption = "Treaty Termination Type,
                         source: https://www.srdlawnotes.com/2017/08/termination-of-treaties.html")
  } else {
    if (is.null(text)) {
    # Step one: if the term date is mentioned in the treaty title,
    # it should be classified as "Sunset/Expiry"
    title <- as.character(title)
    # All the treaties having the term date in the title had the
    # word "For the Period"
    type <- ifelse(grepl("For The Period", title) &
                     grepl("\\d{1,2}.\\w{3,}.\\d{4} To \\d{1,2}.\\w{3,}.\\d{4}",
                           title, ignore.case = T), paste0("EXP"), NA)
    } else {
    title <- as.character(title)
    # All the treaties having the term date in the title had the
    # word "For the Period"
    type <- ifelse(grepl("For The Period", title) &
                     grepl("\\d{1,2}.\\w{3,}.\\d{4} To \\d{1,2}.\\w{3,}.\\d{4}",
                           title, ignore.case = T), paste0("EXP"), NA)
    # Get the termination clauses
    term <- read_clauses(standardise_treaty_text(text),
                         article = "termination")
    # Classify them according to key terms
    term <- dplyr::case_when(
      grepl("shall terminate the agreement|shall supersede|shall.*supplant",
            term, ignore.case = T) ~ "SUB",
      grepl("for a period|shall be for.*years|shall apply for.*years|
            |shall be extended through|concluded for a period.*years|
            |will expire on|shall remain in force until|
            |shall remain in force for", term, ignore.case = T) ~ "SUN",
      grepl("agreement.*shall terminate upon.*completion.*project", term,
            ignore.case = T) ~ "SUC",
      grepl("have denounced this convention|shall be dissolved|
            |may decide.*to dissolve|may be dissolved|may.*denounce",
            term, ignore.case = T) ~ "DIS",
      grepl("renounce its membership|renunciation.*by.*party", term,
            ignore.case = T) ~ "REN",
      grepl("may withdraw|any member.*may.*withdraw|party.*may withraw",
            term, ignore.case = T) ~ "WTH",
      grepl("extraordinary events", term, ignore.case = T) ~ "REB",
      grepl("failure of obligation|nonperformance of obligations", term,
            ignore.case = T) ~ "NON",
      grepl("conflict with.*jus cogens", term, ignore.case = T) ~ "JUS",
      grepl("state party existence.*come to.*end", term,
            ignore.case = T) ~ "EXT",
      grepl("incompatibility between.*agreement and UN charter|
            |incompatibility between.*agreement and United Nations charter",
            term, ignore.case = T) ~ "INC",
      grepl("in the case of war.*end", term, ignore.case = T) ~ "WAR",
      grepl("party injurious.*end.*obligations|injured party.*end.*obligations",
            term, ignore.case = T) ~ "INJ", )
    type <- ifelse(!is.na(type), type, term)
    }
  }
  type
}

#' Code treaty termination date
#'
#' Creates a column with the date of termination
#' of the treaty.
#' @param title title column variable.
#' @param text treaty text column variable
#' @return a character vector with the term date of the
#' treaty
#' @examples
#' \dontrun{
#' HUGGO <- dplyr::slice_sample(manyenviron::agreements$HUGGO, n = 200)
#' code_term_date(HUGGO$Title)
#' }
#' @export
code_term_date <- function(title, text = NULL) {
  if (is.null(text)) {
  # Step one: extract term date if present in treaty title
  title <- as.character(title)
  # Treaties with the term date in the title had the word "For the Period"
  date <- ifelse(grepl("For The Period", title),
                 stringi::stri_extract_all_regex(title, "\\d{1,2}.\\w{3,}.\\d{4}"), NA)
  # Extract only the term date
  date <- suppressWarnings(stri_remove_all(date, "^c|\\(|\\)|\""))
  date <- stringi::stri_extract_all_regex(date, "\\d{1,2}.\\w{3,}.\\d{4}$")
  # Standardise the date format
  date <- as.Date(as.character(date), format = "%d %B %Y")
  } else {
    # repeat same operation for the titles
    title <- as.character(title)
    date <- ifelse(grepl("For The Period", title),
                   stringi::stri_extract_all_regex(title, "\\d{1,2}.\\w{3,}.\\d{4}"),
                   NA)
    date <- suppressWarnings(stri_remove_all(date, "^c|\\(|\\)|\""))
    date <- stringi::stri_extract_all_regex(date, "\\d{1,2}.\\w{3,}.\\d{4}$")
    date <- as.Date(as.character(date), format = "%d %B %Y")
  }
  date
}

termination_type <- dplyr::tribble(~type, ~meaning, ~abbreviation,
"Sunset/Expiry","Treaty concluded for a specific period of time","SUN",
"Success/Completion","The main purpose of the treaty has been fulfilled","SUC",
"Mutual Consent - Recession","The Parties decided to dissolve the treaty","DIS",
"Mutual Consent - Substitution","The Parties decide to substitute the old treaty by a new one","SUB",
"Mututal Consent - Renunciation","A Party can renounce its right arising from the treaty","REN",
"Extinction","When one of the Party State's existence come to an end","EXT",
"Incompatibility","Incompatibility between an obligation from an agreement and obligations under the UN Charter","INC",
"War","In a case of a war, the contractual obligations of the Parties come to an end","WAR",
"Withdrawal","When one of the Party withdrawals from the treaty","WTH",
"Injuriousness","When the terms of a treaty become either wholly or party injurious to one of the party states","INJ",
"Nonperformance","In the case of failure or nonperformance of certain essential conditions arising from the treaty","NON",
"Rebus sic stantibus","The doctrine Rebus sic stantibus means if by any unforeseen change, or circumstances an obligation provided for in the treaty should imperil the existence of one of the State and such state has a right to demand and to be released from the contractual obligations","REB",
"Jus Cogens","When a treaty is in conflict with a peremptory norm of general international law","JUS"
)