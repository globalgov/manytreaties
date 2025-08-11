#' Code Known Agreements Abbreviation
#' @description
#'   Some agreements have known abbreviations that facilitate their identification.
#' @param title A character vector of treaty title
#' @return A character vector of abbreviation of known treaties
#' @importFrom dplyr case_when
#' @importFrom purrr map
#' @details The function identifies agreements that match the list of known
#'   agreements with their titles, abbreviations and signature dates and
#'   substitutes the known titles for abbreviations. For the complete list of
#'   known agreements coded for and their respective abbreviations please run
#'   the function without an argument (i.e. `code_known_agreements()`).
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
    
    # 1: get abbreviations dataset ####
    abbreviations <- purrr::map(abbreviations, as.character)
    
    # 2: assign the specific abbreviation to the "known" treaties ####
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
    
    # 3: keep year only for IDs ####
    out <- ifelse(is.na(out), out, substr(out, 1, nchar(out) - 4))
    
    # 4: if all missing, returns an empty list ####
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

#' Code Acronyms from Titles
#' @description
#'   Codes an acronym from agreement titles to enable matching.
#' @param title A character vector of treaty title
#' @importFrom tm stopwords removeWords
#' @details Codes acronyms that are 4 to 6 digits long.
#'   For shorter treaty titles, six words or less, acronym includes first letter
#'   of each word.
#'   For longer treaty titles, seven words or more, acronym includes first letter
#'   of first word in title, followed by the number of words in the title,
#'   and first letter of last word in title.
#' @examples
#' \dontrun{
#' IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
#' code_acronym(IEADB$Title)
#' }
#' @export
code_acronym <- function(title) {
  
  # 1: Remove stop words ####
  x <- stri_squish(tm::removeWords(title,
             stringi::stri_trans_totitle(tm::stopwords("SMART"))))
  
  # 2: Remove agreement types, numbers, and punctuation marks ####
  x <- stringi::stri_replace_all_regex(x, 
  "protocols|protocol|amendments|amendment|amend|
            |amending|agreements|Agreement|convention|
            |Exchanges|Exchange|Notes|Strategy|strategies|
            |resolutions|Resolution", "", case_insensitive = TRUE)
  x <- stringi::stri_replace_all_regex(x, "\\s\\([:alpha:]{3,9}\\)", "")
  x <- stringi::stri_replace_all_regex(x, "\\s\\(.{3,20}\\)", "")
  x <- stringi::stri_replace_all_regex(x, "[0-9]", "")
  x <- stringi::stri_replace_all_regex(x, "\\(|\\)", "")
  x <- stringi::stri_replace_all_fixed(x, "-", " ")
  
  # 3: remove known agreement cities or short titles ####
  x <- gsub("\\<Nairobi\\>|\\<Basel\\>|\\<Bamako\\>|\\<Lusaka\\>|
            |\\<Stockholm\\>|\\<Kyoto\\>|\\<Hong Kong\\>", "", x)
  x <- ifelse(grepl("^Fisheries", x), gsub("Fisheries", "", x), x)
  
  # 4: remove unimportant but differentiating words ####
  x <- gsub("\\<populations\\>|\\<basin\\>|\\<resources\\>|\\<stock\\>|
  # 4: Remove unimportant but differentiating words ####
  x <- stringi::stri_replace_all_regex(x, "\\<populations\\>|\\<basin\\>|\\<resources\\>|\\<stock\\>|
            |\\<concerning\\>|\\<priority\\>|\\<revised\\>|\\<version\\>|
            |\\<national\\>|\\<trilateral\\>|\\<multilateral\\>|\\<between\\>|
            |\\<marine\\>|\\<Fao\\>|\\<field\\>|\\<sphere\\>|\\<adjustment\\>|
            |\\<activities\\>", "")
  
  # 5: Abbreviate remaining words ####
  x <- suppressWarnings(abbreviate(x, minlength = 6, method = "both.sides",
                                   strict = TRUE))
  x <- toupper(x)
  
  # 6: Condense longer abbreviations to four digits ####
  x <- ifelse(stringr::str_detect(x, "[:upper:]{7}"),
              paste0(substr(x, 1, 2),
                     stringr::str_pad(nchar(x) - 3, 2, pad = "0"),
                     substr(x, nchar(x) - 1, nchar(x))), x)
  cli::cli_alert_success("Coded acronyms")
  as.character(x)
}

