#' Code Agreement Titles
#'
#' Creates an ID that contains information on the parties to an agreement,
#' the type of agreement, the date, and the linkage to other agreements.
#' @param dataset A 'many' dataset.
#' If provided without a title and date variables, the function finds title and
#' date conforming columns in the dataset.
#' The function "expects" that there are variables named `Title` and `Begin`
#' that they have been standardised using `standardise_titles()` and
#' `messydates::as_messydate()`, respectively.
#' @param title A title variable.
#' The function "expects" that the variable has been standardised using
#' `standardise_titles()`.
#' @param date A date variable.
#' The function "expects" that the variable has been standardised using
#' `messydates::as_messydate()`.
#' @return a character vector with the treatyIDs
#' @importFrom usethis ui_done
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
      usethis::ui_done(
        "Title and date conforming columns in dataset automatically found")
    } else if (!exists("Title", dataset) | !exists("Begin", dataset)) {
      stop("Unable to find both 'Title' and 'Begin' columns in dataset.
         Please declare the name of these columns or rename them.")
    }
  }
  # Step 1: get parties, acronym, type, dates, and lineage with code_linkage()
  line <- code_linkage(title, date, return_all = TRUE)
  usethis::ui_done("Coded agreement linkages")
  # Get variables from returned table
  abbrev <- line$abbrev
  type <- line$type
  parties <- line$parties
  acronym <- line$acronym
  uID <- line$uID
  line <- line$line
  # Step 2: add items together correctly
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
  treatyID <- stringr::str_remove_all(treatyID, "_$")
  treatyID <- stringr::str_remove_all(treatyID, ":$")
  # step 3: inform users about observations not matched and duplicates
  cat(sum(is.na(treatyID)), "entries were not matched at all.\n")
  cat("There were", sum(duplicated(treatyID,
                                   incomparables = NA)), "duplicated IDs.\n")
  usethis::ui_done("Please run `vignette('agreements')` for more information.")
  treatyID
}

#' Code Abbreviations for Activity
#'
#' Code in abbreviated form the activity of bilateral treaties' titles.
#' @param title A character vector of treaty titles
#' @details Bilateral agreements usually detail their activity and specify area
#' in the last words of the titles.
#' These last words are abbreviated by the function to differentiate between
#' bilateral treaties and avoid false positives being generated since multiple,
#' different, bilateral treaties are often signed in the same day.
#' @importFrom stringr str_squish str_extract
#' @importFrom tm stopwords removeWords
#' @return A character vector of abbreviations of last words in treaty title.
code_activity <- function(title) {
  # Step 1: remove states' names and agreements' type
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
  # Step 2: remove stop words, numbers and parenthesis
  out <- tm::removeWords(tolower(out), tm::stopwords("SMART"))
  out <- gsub("[0-9]", "", out)
  out <- gsub("\\(|\\)|\U00AC|\U00F1 ", "", out)
  out <- gsub("-", " ", out)
  # Step 3: remove months and unimportant words
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
  # Step 4: get abbreviations for last three words and counting of words
  out <- stringr::str_squish(out)
  out <- suppressWarnings(abbreviate(out, minlength = 3,
                                     method = "both.sides", strict = TRUE))
  out <- stringr::str_extract(out, ".{3}$")
  out <- toupper(out)
  out
}

#' Code Agreement Type
#'
#' Identify the type of international agreement from titles.
#' Agreements can be, for example,
#' multilateral treaties or coventions (A),
#' protocols (P) or amendments (E),
#' if they contain words in title.
#' @param title A character vector of treaty title
#' @return A character vector of the treaty type
#' @importFrom dplyr case_when
#' @importFrom stringr str_extract str_replace_na
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
    # Step 1: get type codes
    out <- purrr::map(title, as.character)
    type <- as.data.frame(agreement_type)
    # Step 2: substitute matching words for categories
    for (k in seq_len(nrow(type))) {
      out <- gsub(paste0(type$terms[[k]]),
                  paste0(type$category[[k]]),
                  out, ignore.case = TRUE,
                  perl = T)
    }
    # Step 3: extract only first category identified
    type <- stringr::str_extract(out,
                                 "PROTO|AMEND|AGREE|NOTES|STRAT|RESOL")
    # Step 4: assign type abbreviations
    type <- dplyr::case_when(
      grepl("PROTO", type) ~ "P", # protocol
      grepl("AMEND", type) ~ "E", # amendment
      grepl("AGREE", type) ~ "A", # agreement
      grepl("NOTES", type) ~ "N", # notes
      grepl("STRAT", type) ~ "S", # strategy
      grepl("RESOL", type) ~ "R", # resolution
    )
    # Step 5: extracts meaningful ordering numbers for protocols and amendments
    number <- order_agreements(title)
    # Assign other (O) no type is found
    type <- stringr::str_replace_na(type, "O")
    # Add type and number if available
    type <- ifelse(type != "A", paste0(type, number), type)
  }
  type
}

agreement_type <- dplyr::tribble(~type,~category,~terms,
                                 "P","PROTO","protocol|additional|subsidiary|supplementary|complementaire|complementar|complementario|annex |annexes",
                                 "E","AMEND","amendment|modify|extend|proces-verbal|amend|extension|agreement extending|amending",
                                 "A","AGREE","agreement|arrangement|accord|acuerdo|bilateral co|technical co|treat|trait|tratado|convention|convencion|convenio|constitution|charte|instrument|statute|estatuto|provisional understanding|provisions relating|ubereinkunft|Act|Covenant|Scheme|Government Of|Law",
                                 "N","NOTES","Exchange|Letters|Notas|Minute|Adjustment|First Session Of|First Meeting Of|Commission|Committee|Center",
                                 "S","STRAT","Memorandum|memorando|Principles of Conduct|Code of Conduct|Strategy|Plan|Program|Improvement|Project|Study|Working Party|Working Group",
                                 "R","RESOL","Agreed Measures|Agreed Record|Consensus|Conclusions|Decision|Directive|Regulation|Reglamento|Resolution|Rules|Recommendation|Statement|Communiq|Comminiq|Joint Declaration|Declaration|Proclamation|Administrative Order"
)

#' Creates Numerical IDs from Signature Dates
#'
#' Agreements should have a unique identification
#' number that is meaningful, we condense their
#' signature dates to produce this number.
#' @param date A date variable
#' @return A character vector with condensed dates
#' @import stringr
#' @examples
#' \dontrun{
#' IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
#' code_dates(IEADB$Title)
#' }
#' @export
code_dates <- function(date) {
  # Step 1: collapse dates
  uID <- stringr::str_remove_all(date, "-")
  # Step 2: code missing dates as far future dates to facilitate identification
  uID[is.na(uID)] <- paste0(sample(5000:9999, 1), "NULL")
  # Step 3: remove ranges, first date is taken
  uID <- stringr::str_replace_all(uID, "\\:[:digit:]{8}$", "")
  # Step 4: keep year only
  uID <- ifelse(nchar(uID) > 4, substr(uID, 1, nchar(uID) - 4), uID)
  uID
}

#' Code Known Agreements Abbreviation
#'
#' Some agreements have known abbreviations that facilitate their identification.
#' @param title A character vector of treaty title
#' @return A character vector of abbreviation of known treaties
#' @importFrom dplyr case_when
#' @importFrom purrr map
#' @details The function identifies agreements that match the list of known
#' agreements with their titles, abbreviations and signature dates and
#' substitutes the known titles for abbreviations.For the complete list of
#' known agreements coded for and their respective abbreviations please run
#' the function without an argument (i.e. `code_known_agreements()`).
#' @examples
#' \dontrun{
#' IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
#' code_known_agreements(IEADB$Title)
#' }
#' @export
code_known_agreements <- function(title) {
  if (missing(title)) {
    # If missing argument, function returns list of known agreements coded
    ka <- as.data.frame(abbreviations)
    ka$title[15] <- paste(substr(ka$title[15], 0, 90), "...")
    ka$title[17] <- paste(substr(ka$title[17], 0, 90), "...")
    out <- knitr::kable(ka, "simple")
  } else {
    # Step 1: get abbreviations dataset
    abbreviations <- purrr::map(abbreviations, as.character)
    # Step 2: assign the specific abbreviation to the "known" treaties
    ab <- sapply(abbreviations$title, function(x) grepl(x, title,
                                                        ignore.case = T,
                                                        perl = T) * 1)
    colnames(ab) <- paste0(abbreviations$abbreviation, "_",
                           as.character(stringr::str_remove_all(
                             abbreviations$signature, "-")))
    rownames(ab) <- title
    out <- apply(ab, 1, function(x) paste(names(x[x == 1])))
    # Assign NA when observation is not matched
    out[out == "character(0)"] <- NA_character_
    out <- unname(out)
    out <- as.character(out)
    out <- ifelse(grepl("c\\(", out), "PARIS_20151212", out)
    # Step 3: keep year only for IDs
    out <- ifelse(is.na(out), out, substr(out, 1, nchar(out) - 4))
    # Step 4: if all missing, returns an empty list
    lt <- as.numeric(length(title))
    ifelse(length(out) == 0, out <- rep(NA_character_, lt), out)
  }
  out
}

abbreviations <- dplyr::tribble(~title,~abbreviation,~signature,
  "United Nations Convention On The Law Of The Sea","UNCLOS","1982-12-10",
  "Convention On Biological Diversity","CBD","1992-06-05",
  "Convention On The Conservation Of Antarctic Marine Living Resources","CCAMLR","1980-05-20",
  "Convention On International Trade In Endangered Species Of Wild Fauna And Flora","CITES","1973-03-03",
  "International Convention On Civil Liability For Oil Pollution Damage","CLC","1969-11-29",
  "Antarctic Mineral Resources Convention","CRAMRA","1988-06-02",
  "Convention On The Protection And Use Of Transboundary Watercourses And International Lakes","CECE","1992-03-17",
  "Convention On Long-Range Transboundary Air Pollution","LRTAP","1979-11-13",
  "International Convention For The Prevention Of Pollution From Ships","MARPOL","1973-11-02",
  "North American Agreement On Environmental Cooperation","NAAEC","1993-09-14",
  "Constitutional Agreement Of The Latin American Organization For Fisheries Development","OLDEPESCA","1982-10-29",
  "International Convention On Oil Pollution Preparedness, Response And Cooperation","OPRC","1990-11-30",
  "Convention For The Protection Of The Marine Environment Of The North East Atlantic","OSPAR","1992-09-22",
  "Paris Agreement Under The United Nations Framework Convention On Climate Change","PARIS","2015-12-12",
  "Convention On The Prior Informed Consent Procedure For Certain Hazardous Chemicals And Pesticides In International Trade","PIC","1998-09-10",
  "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat","RAMSA","1971-02-02",
  "Convention To Combat Desertification In Those Countries Experiencing Serious Drought And/Or Desertification, Particularly In Africa","UNCCD","1994-06-17",
  "United Nations Framework Convention On Climate Change","UNFCCC","1992-05-09",
  "Convention For The Protection Of The Ozone Layer","VIENNA","1985-03-22"
)

#' Code Acronym for Titles
#'
#' Codes an acronym for agreement titles to facilitate comparison.
#' @param title A character vector of treaty title
#' @import stringr
#' @importFrom tm stopwords removeWords
#' @details Codes acronyms that are 4 to 6 digits long.
#' For shorter treaty titles, six words or less, acronym includes first letter
#' of each word.
#' For longer treaty titles, seven words or more, acronym includes first letter
#' of first word in title, followed by the number of words in the title,
#' and first letter of last word in title.
#' @examples
#' \dontrun{
#' IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
#' code_acronym(IEADB$Title)
#' }
#' @export
code_acronym <- function(title) {
  # Step 1: standardise titles
  x <- standardise_titles(tm::removeWords(tolower(title),
                                          tm::stopwords("SMART")))
  # Step 2: remove agreement types, numbers, and punctuation marks
  x <- gsub("protocol|protocols|amendment|amendments|amend|
            |amending|Agreement|agreements|convention|
            |Exchange|Exchanges|Notes|Strategy|strategies|
            |Resolution|resolutions", "", x, ignore.case = TRUE)
  x <- stringr::str_remove_all(x, "\\s\\([:alpha:]{3,9}\\)")
  x <- stringr::str_remove_all(x, "\\s\\(.{3,20}\\)")
  x <- stringr::str_remove_all(x, "[0-9]")
  x <- stringr::str_remove_all(x, "\\(|\\)")
  x <- gsub("-", " ", x)
  # Step 3: remove known agreement cities or short titles
  x <- gsub("\\<Nairobi\\>|\\<Basel\\>|\\<Bamako\\>|\\<Lusaka\\>|
            |\\<Stockholm\\>|\\<Kyoto\\>|\\<Hong Kong\\>", "", x)
  x <- ifelse(grepl("^Fisheries", x), gsub("Fisheries", "", x), x)
  # Step 4: remove unimportant but differentiating words
  x <- gsub("\\<populations\\>|\\<basin\\>|\\<resources\\>|\\<stock\\>|
            |\\<concerning\\>|\\<priority\\>|\\<revised\\>|\\<version\\>|
            |\\<national\\>|\\<trilateral\\>|\\<multilateral\\>|\\<between\\>|
            |\\<marine\\>|\\<Fao\\>|\\<field\\>|\\<sphere\\>|\\<adjustment\\>|
            |\\<activities\\>", "", x, ignore.case = TRUE)
  # Step 5: get abbreviations for words left
  x <- suppressWarnings(abbreviate(x, minlength = 6, method = "both.sides",
                                   strict = TRUE))
  x <- toupper(x)
  # Step 6: cut longer abbreviations into four digits
  x <- ifelse(stringr::str_detect(x, "[:upper:]{7}"),
              paste0(substr(x, 1, 2),
                     stringr::str_pad(nchar(x) - 3, 2, pad = "0"),
                     substr(x, nchar(x) - 1, nchar(x))), x)
  x <- as.character(x)
  x
}

#' Code Agreement Linkages
#'
#' Identify the linkage between amendments and protocols to a main agreement.
#' @param title A character vector of treaty title
#' @param date A date variable
#' @param return_all Do you want all the variables to be returned in a list?
#' By default, FALSE.
#' @importFrom stringr str_replace_all str_squish str_remove_all
#' @importFrom purrr map
#' @import dplyr
#' @return A character vector of the agreements that are linked
#' @details The function identifies duplicates by excluding
#' "predictable" words from strings, this maintains key words then used
#' to identify and link duplicates.
#' This is a choice that considers errors should lie on the side of false
#' negatives rather than false positives.
#' For the complete list of words removed from title to identify duplicates
#' please run the function without arguments (i.e. `code_linkage()`).
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
    # Step 1: standardise titles to improve matching
    treaty <- standardise_titles(as.character(title))
    # Step 2: code parties if present
    parties <- manystates::code_states(treaty)
    usethis::ui_done("Coded agreement parties")
    # Step 3: code agreement type
    type <- code_type(treaty)
    usethis::ui_done("Coded agreement type")
    # Step 4: code known agreements
    abbrev <- code_known_agreements(treaty)
    usethis::ui_done("Coded known agreements")
    # Step 5: give the observation a unique ID and acronym
    uID <- code_dates(date)
    usethis::ui_done("Coded agreement dates")
    # Step 6: code acronyms from titles
    acronym <- code_acronym(title)
    usethis::ui_done("Coded acronyms for agreements")
    # Step 7: remove 'predictable words' in agreements
    pw <- paste0("\\<", paste(predictable_words$predictable_words,
                              collapse = "\\>|\\<"), "\\>")
    treaty <- gsub(pw, "", treaty, ignore.case = TRUE)
    # Step 8: remove numbers, signs and parentheses
    treaty <- gsub("\\s*\\([^\\)]+\\)", "", treaty, ignore.case = FALSE)
    treaty <- gsub("-", " ", treaty, ignore.case = FALSE)
    treaty <- stringr::str_replace_all(treaty, ",", "")
    treaty <- stringr::str_remove_all(treaty, "[0-9]")
    treaty <- data.frame(treaty = stringr::str_squish(treaty))
    # Step 9: assign ID to observations
    id <- ifelse((!is.na(abbrev)), paste0(abbrev, "A"),
                 (ifelse((is.na(parties)), paste0(acronym, "_", uID, type),
                         (ifelse((!is.na(parties)), paste0(parties, "_", uID,
                                                           type), NA)))))
    # Step 10: bind, arrange, find duplicates, original values, and assign same id
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
    # Step 11: keep only linkages for agreements
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
