#' Code Agreement Titles
#' @description
#'   Creates an ID that contains information on the parties to an agreement,
#'   the type of agreement, the date, and the linkage to other agreements.
#' @param dataset A 'many' dataset.
#'   If provided without a title and date variables, the function finds title 
#'   and date conforming columns in the dataset.
#'   The function expects that there are variables named `Title` and `Begin`
#'   that they have been standardised using `standardise_titles()` and
#'   `messydates::as_messydate()`, respectively.
#' @param title A title variable.
#'   The function expects that the variable has been standardised using
#'   `standardise_titles()`.
#' @param date A date variable.
#'   The function expects that the variable has been standardised using
#'   `messydates::as_messydate()`.
#' @return a character vector with the treatyIDs
#' @importFrom stringr str_replace_all str_detect
#' @importFrom purrr map
#' @examples
#' \dontrun{
#' IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
#' code_agreements(dataset = IEADB)
#' code_agreements(title = IEADB$Title, date = IEADB$Begin)
#' }
#' @export
code_agreements <- function(dataset = NULL, title, date) {
  
  # Check requirements
  if (is.null(dataset) & missing(title) & missing(date)) {
    stop("Please declare a dataset or title and date columns.")
  }
  if (!is.null(dataset) & missing(title) & missing(date)) {
    if (exists("Title", dataset) & exists("Begin", dataset)) {
      title <- dataset$Title
      date <- dataset$Begin
      cli::cli_inform(
        "Title and date conforming columns in dataset found automagically.")
    } else if (!exists("Title", dataset) | !exists("Begin", dataset)) {
      cli::cli_abort("Unable to find both 'Title' and 'Begin' columns in dataset.
         Please declare the name of these columns or rename them.")
    }
  }
  
  # 1: Obtain parties, acronym, type, dates, and lineage with code_linkage() ####
  line <- code_linkage(title, date, return_all = TRUE)
  # cli::cli_alert_success("Coded agreement linkages")
  # Get variables from returned table
  abbrev <- line$abbrev
  type <- line$type
  parties <- line$parties
  acronym <- line$acronym
  uID <- line$uID
  line <- line$line
  
  # 2: Add items together correctly ####
  out <- vector(mode = "character", length = length(title)) # initialize vector
  # bilateral agreements (A) where abbreviation is known
  treatyID <- ifelse(!is.na(abbrev) & (type == "A") & !is.na(parties),
                paste0(parties, "_", uID, type, ":", abbrev), out)
  treatyID <- ifelse(!is.na(abbrev) & (type != "A") & !is.na(parties),
                paste0(parties, "_", uID, type, ":", line), treatyID)
  # multilateral agreements (A) where abbreviation is known
  treatyID <- ifelse(!is.na(abbrev) & (type == "A") & is.na(parties),
                paste0(abbrev, type), treatyID)
  # when abbreviation is known but treaty type is not agreement
  treatyID <- ifelse(!is.na(abbrev) & (type != "A") & is.na(parties),
                paste0(acronym, "_", uID, type, ":", line), treatyID)
  # when parties were not identified and treaty type is agreement (A)
  treatyID <- ifelse(is.na(parties) & (type == "A") & is.na(abbrev),
                paste0(acronym, "_", uID, type), treatyID)
  # when parties were not identified and type is not agreement
  treatyID <- ifelse(is.na(parties) & (type != "A") & is.na(abbrev),
                paste0(acronym, "_", uID, type, ":", line), treatyID)
  # when parties were identified and type is agreement (A)
  treatyID <- ifelse(!is.na(parties) & (type == "A") & is.na(abbrev),
                paste0(parties, "_", uID, type), treatyID)
  # when parties were identified and type is not agreement
  treatyID <- ifelse(!is.na(parties) & (type != "A") & is.na(abbrev),
                paste0(parties, "_", uID, type, ":", line), treatyID)
  # deletes empty line or linkage
  treatyID <- stringi::stri_replace_all_regex(treatyID, "_$|:$", "")

  # 3: Inform users about observations not matched and duplicates ####
  if(sum(is.na(treatyID)) > 0) {
    cli::cli_alert_danger(sum(is.na(treatyID)), "entries were unmatched.")
  }
  if(sum(duplicated(treatyID, incomparables = NA)) > 0) {
    cli::cli_alert_warning(sum(duplicated(treatyID, incomparables = NA)),
                           "entries were duplicated.")
  }
  cli::cli_inform("Please run `vignette('agreements')` for more information.")
  treatyID
}

#' Code Agreement Linkages
#' @description
#'   Identify the linkage between amendments and protocols to a main agreement.
#' @param title A character vector of treaty title
#' @param date A date variable
#' @param return_all Do you want all the variables to be returned in a list?
#'   By default, FALSE.
#' @importFrom stringr str_replace_all str_squish str_remove_all
#' @importFrom purrr map
#' @import dplyr
#' @return A character vector of the agreements that are linked
#' @details The function identifies duplicates by excluding
#'   "predictable" words from strings, this maintains key words then used
#'   to identify and link duplicates.
#'   This is a choice that considers errors should lie on the side of false
#'   negatives rather than false positives.
#'   For the complete list of words removed from title to identify duplicates
#'   please run the function without arguments (i.e. `code_linkage()`).
#' @examples
#' \dontrun{
#' IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
#' code_linkage(IEADB$Title, IEADB$Begin)
#' }
#' @export
code_linkage <- function(title, date, return_all = FALSE) {
  
  # Initialize variables to suppress CMD notes
  ref <- dup <- NULL
  if (missing(title) & missing(date)) {
    pred <- as.data.frame(predictable_words)
    pred_words <- knitr::kable(pred, "simple")
    pred_words
  } else {
    
    treaty <- as.character(title)
    
    # 1: Code parties if present ####
    parties <- manystates::code_states(treaty, max_count = 2)
    parties <- stringi::stri_replace_all_fixed(parties, ",", "-")
    parties <- stringi::stri_replace_all_regex(parties, "\\{|\\}", "")
    cli::cli_alert_success("Coded agreement parties")
    
    # 2: Code agreement type ####
    type <- code_type(treaty)
    
    # 3: Give observation a unique ID and acronym ####
    uID <- as.character(messydates::year(date))
    cli::cli_alert_success("Coded agreement dates")
    
    # 4: Code acronyms for known and unknown agreements ####
    abbrev <- code_known_agreements(treaty)
    acronym <- code_acronym(treaty)
    
    # 6: Remove 'predictable words' in agreements ####
    pw <- paste0("\\<", paste(predictable_words$predictable_words,
                              collapse = "\\>|\\<"), "\\>")
    treaty <- gsub(pw, "", treaty, ignore.case = TRUE)
    
    # 7: Remove numbers, signs and parentheses ####
    treaty <- gsub("\\s*\\([^\\)]+\\)", "", treaty, ignore.case = FALSE)
    treaty <- gsub("-", " ", treaty, ignore.case = FALSE)
    treaty <- stringi::stri_replace_all_fixed(treaty, ",", "")
    treaty <- stringi::stri_replace_all_regex(treaty, "[0-9]", "")
    treaty <- data.frame(treaty = stringr::str_squish(treaty))
    
    # 8: Assign ID to observations ####
    id <- ifelse((!is.na(abbrev)), paste0(abbrev, "A"),
                 (ifelse((is.na(parties)), paste0(acronym, "_", uID, type),
                         (ifelse((!is.na(parties)), paste0(parties, "_", uID,
                                                           type), NA)))))
    
    # 9: Bind, arrange, find duplicates, original values, and assign same id ####
    out <- cbind(treaty, id, parties, type, abbrev, uID, acronym) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      dplyr::arrange(type) %>%
      dplyr::group_by_at(dplyr::vars(treaty)) %>%
      dplyr::mutate(dup = dplyr::row_number() > 1,
                    ref = ifelse(dup, paste0(dplyr::first(id)),
                                 as.character(id))) %>%
      dplyr::group_by(ref) %>%
      dplyr::mutate(n = dplyr::n(),
                    line = dplyr::case_when(n != 1 ~ paste(ref),
                                            n == 1 ~ "1")) %>%
      dplyr::arrange(row)
    
    # 10: Keep only linkages for agreements ####
    out$line <- ifelse(out$id == out$ref & out$type == "A", "1", out$line)
    out$line <- stringr::str_replace_all(out$line, "^1$", "")
    out$line <- stringr::str_replace_all(out$line,
                                         "[0-9]{4}E|[0-9]{4}P|[0-9]{4}S|[0-9]{4}N|[0-9]{4}R|[0-9]{4}O",
                                         "xxxxxxxxxxxxxxxxxxxxXx")
    out$line <- ifelse(nchar(as.character(out$line)) > 20, "", out$line)
    if (return_all == FALSE) {
      out <- out$line
    }
    out
  }
}

predictable_words <- dplyr::tribble(~predictable_words,
"amendment",
"amendments",
"amend",
"amending",
"modifying",
"modify",
"extension",
"extend",
"extending",
"verbal",
"protocol",
"additional",
"subsidiary",
"supplementary",
"complementary",
"complementario",
"agreement",
"agreements",
"arrangement",
"arrangements",
"accord",
"acuerdo",
"bilateral",
"technical",
"treaty",
"trait",
"tratado",
"convention",
"convencion",
"convenio",
"constitution",
"charte",
"instrument",
"statute",
"estatuto",
"provisional",
"understanding",
"provisions",
"relating",
"ubereinkunft",
"Act",
"Acts",
"Declaration",
"Covenant",
"Scheme",
"Government Of |Law",
"Exchange",
"Letters",
"Letter",
"Notas",
"Notes",
"Memorandum",
"memorando",
"Principles of Conduct",
"Code of Conduct",
"Agreed Measures",
"Agreed Record",
"Consensus",
"Conclusions",
"Conclusion",
"Decision",
"Directive",
"Regulation",
"Reglamento",
"Resolution",
"Resolutions",
"Rule",
"Rules",
"Recommendation",
"Minute",
"Adjustment",
"First|Session Of",
"First Meeting Of",
"Commission",
"Committee",
"Center",
"Meeting",
"Meetings",
"Statement",
"Communiq",
"Comminiq",
"Joint Declaration",
"Proclamation",
"Administrative Order",
"Strategy",
"Plan",
"Program",
"Improvement",
"Project",
"Study",
"Article",
"Articles",
"Working Party",
"Working Group",
"Supplementary",
"supplementing",
"Annex",
"Annexes",
"extended",
"Constitutional",
"Constituent",
"A",
"B",
"C",
"D",
"E",
"F",
"G",
"H",
"I",
"J",
"K",
"L",
"M",
"N",
"O",
"P",
"Q",
"R",
"S",
"T",
"U",
"V",
"W",
"X",
"Y",
"Z",
"and",
"the",
"of",
"for",
"to",
"in",
"a",
"an",
"on",
"the",
"as",
"optional",
"concerning",
"compulsory",
"Settlement",
"disputes",
"dispute",
"schedule",
"adhesion",
"implementation",
"government",
"Cooperation",
"Appendices",
"integrated",
"integrating",
"Signature",
"terminating",
"Appendices",
"integrated",
"integrating",
"Signature",
"terminating",
"or",
"between",
"constituing",
"between",
"constituing",
"january",
"february",
"march",
"april",
"may",
"june",
"july",
"august",
"september",
"october",
"november",
"december",
"about",
"river"
)

order_agreements <- function(title) {
  # Step 1: remove dates signs title
  title <- stringr::str_replace_all(title, " \\- ", "")
  title <- stringr::str_replace_all(title, "\\-|\\/", " ")
  title <- stringr::str_squish(title)
  # Step 2: remove dates from title
  rd <- stringr::str_remove_all(title, "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|
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
  rd <- stringr::str_remove_all(rd, "\\s\\(No\\s.{3,7}\\)")
  # Step 3: standardises ordinal numbers and ordering text into digits
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
  # Step 4: make sure meaningful numbers extracted correctly
  oa <- stringr::str_extract(oa, "\\s[:digit:]{1}\\s|\\s[:digit:]{2}\\s|\\s[:digit:]{2}|
                             |[:digit:]{2}\\s|\\s[:digit:]{1}|[:digit:]{1}\\s")
  oa <- stringr::str_replace_all(oa, "\\s", "")
  oa <- stringr::str_replace_na(oa)
  oa <- stringr::str_remove_all(oa, "NA")
  oa
}

