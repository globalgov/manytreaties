#' Code Agreement Type
#' @description
#'   Identify the type of international agreement from titles.
#'   Agreements can be, for example, treaties or conventions (A),
#'   protocols (P), or amendments (E),
#'   if they contain words in title.
#' @param title A character vector of treaty title
#' @return A character vector of the treaty type
#' @importFrom dplyr case_when
#' @importFrom purrr map
#' @importFrom knitr kable
#' @details Types of agreements differentiate agreements
#' from protocols or amendments, for example.
#' For the complete list of types of agreements coded
#' please run the function without an argument
#' (i.e. `code_type()`).
#' @examples
#' \dontrun{
#' IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
#' code_type(IEADB$Title)
#' }
#' @export
code_type <- function(title) {
  if (missing(title)) {
    # If missing argument, returns list of types and words coded
    type <- as.data.frame(agreement_type)
    type$words[3] <- paste(substr(type$words[3], 0, 120), "...")
    type$words[5] <- paste(substr(type$words[5], 0, 120), "...")
    type$words[6] <- paste(substr(type$words[6], 0, 120), "...")
    type <- knitr::kable(type, "simple")
  } else {
    
    # 1: Substitute matching words for categories ####
    out <- purrr::map(title, as.character)
    type <- as.data.frame(agreement_type)
    for (k in seq_len(nrow(type))) {
      out <- gsub(paste0(type$terms[[k]]),
                  paste0(type$category[[k]]),
                  out, ignore.case = TRUE,
                  perl = T)
    }
    
    # 2: Extract only first category identified ####
    type <- stringi::stri_extract_first_regex(out,
                                 "PROTO|AMEND|AGREE|NOTES|STRAT|RESOL")
    
    # 3: Assign type abbreviations ####
    type <- dplyr::case_when(
      grepl("AMEND", type) ~ "E", # amendment
      grepl("PROTO", type) ~ "P", # protocol
      grepl("AGREE", type) ~ "A", # agreement
      grepl("NOTES", type) ~ "N", # notes
      grepl("STRAT", type) ~ "S", # strategy
      grepl("RESOL", type) ~ "R", # resolution
    )
    # Assign other (O) no type is found
    type <- stringi::stri_replace_na(type, "O")
    
    # 4: Extracts meaningful ordering numbers for protocols and amendments ####
    number <- order_agreements(title)
    # Add type and number if available
    type <- ifelse(type != "A", paste0(type, number), type)
  }
  cli::cli_alert_success("Coded agreement type")
  type
}

# Agreement type data ####
agreement_type <- dplyr::tribble(~type,~category,~terms,
                                 "P","PROTO","protocol|additional|subsidiary|supplementary|complementaire|complementar|complementario|annex |annexes",
                                 "E","AMEND","amendment|modify|extend|proces-verbal|amend|extension|agreement extending|amending",
                                 "A","AGREE","agreement|arrangement|accord|acuerdo|bilateral co|technical co|treat|trait|tratado|convention|convencion|convenio|constitution|charte|instrument|statute|estatuto|provisional understanding|provisions relating|ubereinkunft|Act|Covenant|Scheme|Government Of|Law",
                                 "N","NOTES","Exchange|Letters|Notas|Minute|Adjustment|First Session Of|First Meeting Of|Commission|Committee|Center",
                                 "S","STRAT","Memorandum|memorando|Principles of Conduct|Code of Conduct|Strategy|Plan|Program|Improvement|Project|Study|Working Party|Working Group",
                                 "R","RESOL","Agreed Measures|Agreed Record|Consensus|Conclusions|Decision|Directive|Regulation|Reglamento|Resolution|Rules|Recommendation|Statement|Communiq|Comminiq|Joint Declaration|Declaration|Proclamation|Administrative Order"
)

order_agreements <- function(title) {
  
  # 1: Remove dates signs title ####
  title <- stri_remove_all(title, " \\- ")
  title <- stri_remove_all(title, "\\-|\\/")
  title <- stri_squish(title)
  
  # 2: Remove dates from title ####
  rd <- stri_remove_all(title, "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|
                                |[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}|
                                |[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|
                                |[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}|
                                |[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|
                                |[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}|
                                |[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|
                                |[:digit:]{1}\\s[:alpha:]{3}\\s[:digit:]{4}|
                                |[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}|
                                |[:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}|
                                |[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}|
                                |[:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}|
                                |[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}|
                                |[:digit:]{1}\\s[:alpha:]{9}\\s[:digit:]{4}|
                                |[:digit:]{4}\\s[:alpha:]{3}\\s[:digit:]{2}|
                                |[:digit:]{4}\\s[:alpha:]{4}\\s[:digit:]{2}|
                                |[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{2}|
                                |[:digit:]{4}\\s[:alpha:]{6}\\s[:digit:]{2}|
                                |[:digit:]{4}\\s[:alpha:]{7}\\s[:digit:]{2}|
                                |[:digit:]{4}\\s[:alpha:]{8}\\s[:digit:]{2}|
                                |[:digit:]{4}\\s[:alpha:]{9}\\s[:digit:]{2}|
                                |[:digit:]{4}| [:digit:]{2}\\s[:digit:]{2}|
                                |[:digit:]{4}\\s[:digit:]{2}\\s[:digit:]{2}|
                                |[:digit:]{4}\\s[:digit:]{2}\\s[:digit:]{1}|
                                |[:digit:]{4}\\s[:digit:]{1}\\s[:digit:]{2}|
                                |[:digit:]{4}\\s[:digit:]{1}\\s[:digit:]{1}|
                                |[:digit:]{2}\\s[:digit:]{2}\\s[:digit:]{4}|
                                |[:digit:]{1}\\s[:digit:]{2}\\s[:digit:]{4}|
                                |[:digit:]{2}\\s[:digit:]{1}\\s[:digit:]{4}|
                                |[:digit:]{1}\\s[:digit:]{1}\\s[:digit:]{4}")
  # remove also numbers in parenthesis
  rd <- stri_remove_all(rd, "\\s\\(No\\s.{3,7}\\)")
  
  # 3: Standardises ordinal numbers and ordering text into digits ####
  oa <- gsub("\\<one\\>|\\<first\\>|  I ", "1", rd)
  oa <- gsub("\\<two\\>|\\<second\\>| Ii ", "2", oa)
  oa <- gsub("\\<three\\>|\\<third\\>| Iii ", "3", oa)
  oa <- gsub("\\<four\\>|\\<fourth\\>| Iv ", "4", oa)
  oa <- gsub("\\<five\\>|\\<fifth\\>| V |No5", "5", oa)
  oa <- gsub("\\<six\\>|\\<sixth\\>|No6", "6", oa)
  oa <- gsub("\\<seven\\>|\\<seventh\\>", "7", oa)
  oa <- gsub("\\<eight\\>|\\<eighth\\>", "8", oa)
  oa <- gsub("\\<nine\\>|\\<ninth\\>", "9", oa)
  oa <- gsub("\\<ten\\>|\\<tenth\\>", "10", oa)
  oa <- gsub("\\<eleven\\>|\\<eleventh\\>", "11", oa)
  oa <- gsub("\\<twelve\\>|\\<twelfth\\>", "12", oa)
  oa <- gsub("\\<thirteen\\>|\\<thirteenth\\>", "13", oa)
  oa <- gsub("\\<fourteen\\>|\\<fourteenth\\>", "14", oa)
  oa <- gsub("\\<fifteen\\>|\\<fifteenth\\>", "15", oa)
  oa <- gsub("\\<sixteen\\>|\\<sixteenth\\>", "16", oa)
  oa <- gsub("\\<seventeen\\>|\\<seventeenth\\>", "17", oa)
  oa <- gsub("\\<eighteen\\>|\\<eighteenth\\>", "18", oa)
  oa <- gsub("\\<nineteen\\>|\\<nineteenth\\>", "19", oa)
  oa <- gsub("\\<twenty\\>|\\<twentieth\\>", "20", oa)
  
  # 4: Ensure meaningful numbers extracted correctly ####
  oa <- stringi::stri_extract_first_regex(oa, "\\s[:digit:]{1}\\s|\\s[:digit:]{2}\\s|\\s[:digit:]{2}|
                             |[:digit:]{2}\\s|\\s[:digit:]{1}|[:digit:]{1}\\s")
  oa <- stri_remove_all(oa, "\\s")
  oa <- stringi::stri_replace_na(oa)
  oa <- stri_remove_all(oa, "NA")
  oa
}

#' Code Abbreviations for Activity
#'
#' Code in abbreviated form the activity of bilateral treaties' titles.
#' @param title A character vector of treaty titles
#' @details Bilateral agreements usually detail their activity and specify area
#'   in the last words of the titles.
#'   These last words are abbreviated by the function to differentiate between
#'   bilateral treaties and avoid false positives being generated since multiple,
#'   different, bilateral treaties are often signed in the same day.
#' @importFrom tm stopwords removeWords
#' @return A character vector of abbreviations of last words in treaty title.
code_activity <- function(title) {
  
  # 1: Remove states' names and agreements' type ####
  out <- as.character(title)
  states <- manystates::code_states()$Label
  states <- paste(states, collapse = "|")
  words <- agreement_type$words
  words <- paste(words, collapse = "|")
  out <- gsub(states, "", out, ignore.case = TRUE)
  out <- gsub(words, "", out, ignore.case = TRUE)
  # Some states and abbreviations are missed
  out <- gsub("Soviet Socialist Republics|\\<USSR\\>|\\<UK\\>|
              |\\<US\\>||\\<united\\>|\\<america\\>",
              "", out, ignore.case = TRUE)
  
  # 2: Remove stop words, numbers and parenthesis ####
  out <- tm::removeWords(tolower(out), tm::stopwords("SMART"))
  out <- gsub("[0-9]", "", out)
  out <- gsub("\\(|\\)|\U00AC|\U00F1 ", "", out)
  out <- gsub("-", " ", out)
  
  # 3: Remove months and unimportant words ####
  out <- gsub("january|february|march|april|may|june|july|
              |august|september|october|november|december",
              "", out, ignore.case = TRUE)
  out <- gsub("\\<text\\>|\\<signed\\>|\\<government\\>|\\<federal\\>|
              |\\<republic\\>|\\<states\\>|\\<confederation\\>|
              |\\<federative\\>|\\<kingdom\\>|\\<republics\\>",
              "", out, ignore.case = TRUE)
  out <- gsub("\\<coast\\>|\\<ocean\\>|\\<eastern\\>|\\<western\\>|
              |\\<north\\>|\\<south\\>|\\<west\\>|\\<east\\>|
              |\\<southern\\>|\\<northern\\>|\\<middle\\>|\\<atlantic\\>|
              |\\<pacific\\>|\\<columbia\\>|\\<danube\\>",
              "", out, ignore.case = TRUE)
  out <- gsub("\\<between\\>|\\<cooperation\\>|\\<cooperative\\>|
              |\\<scientific\\>|\\<technical\\>|\\<basic\\>|\\<border\\>|
              |\\<pollution\\>|\\<river\\>|\\<basin\\>|\\<water\\>|
              |\\<resources\\>|\\<aim\\>|\\<reducing\\>|\\<cross\\>|
              |\\<relating\\>|\\<iron\\>|\\<gates\\>|\\<power\\>|
              |\\<navigation\\>|\\<system\\>|\\<sphere\\>|\\<field\\>|
              |\\<partnership\\>|\\<science\\>|\\<matters\\>",
              "", out, ignore.case = TRUE)
  
  # 4: Get abbreviations for last three words and counting of words ####
  out <- stri_squish(out)
  out <- suppressWarnings(abbreviate(out, minlength = 3,
                                     method = "both.sides", strict = TRUE))
  out <- stringi::stri_extract_first_regex(out, ".{3}$")
  out <- toupper(out)
  out
}

