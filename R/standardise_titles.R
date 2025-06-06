#' Standardise titles
#'
#' Standardises words in a character title variable to improve readability,
#' facilitate string matching and enable more accurate comparisons
#' for variables in different datatsets.
#' @name standardise_titles
#' @param s A string
#' @details The function capitalises words in the strings passed to it.
#' It trims white spaces from the start, middle and end of the strings.
#' Removes ambiguous punctions and symbols from strings.
#' All the strings are transformed into to ASCII character encoding.
#' Written numbers in ordinal form are transformed into numerical form.
#' @return A capitalised, trimmed and standardised string
#' @importFrom textclean add_comma_space mgsub
#' @importFrom english ordinal words
#' @importFrom stringr str_count str_squish str_to_title
#' @importFrom utils as.roman
#' @importFrom stringi stri_trans_general
#' @import dplyr
#' @examples
#' e <- standardise_titles("A treaty concerning things")
#' e==c("A Treaty Concerning Things")
#' @export
standardise_titles <- function(s) {
  # Step one: capitalises first letter in words
  cap <- function(s) paste(toupper(substring(s, 1, 1)), {
    s <- substring(s, 2)
  }
  , sep = "", collapse = " ")
  out <- vapply(strsplit(s, split = " "), cap, "",
                USE.NAMES = !is.null(names(s)))
  # Step two: standardise strings returned
  # Transforms strings to ASCII character encoding
  out <- suppressWarnings(stringi::stri_trans_general(out, id = "Latin-ASCII"))
  # standardises NAs
  out[out == "NANA"] <- NA
  out <- gsub("\\.(?=\\.*$)", "", out, perl = TRUE)
  # standardises spaces before and after apostrophes and comma spaces
  out <- gsub(" '|' ", "'", out)
  # Delete hyphens when separating two parts of the title
  # (when there is a space before and after)
  # Delete special character found in some treaty titles
  out <- gsub("\U00AC[[:alpha:]]{1}\\s|\U00AC\\s| -", "", out)
  # Add space after a comma
  out <- textclean::add_comma_space(out)
  # Change number symbol into word
  out <- gsub("\\#", "Number ", out)
  # standardise some country abbreviations and specific words
  out <- correct_words(out)
  # Step three: Standardises how ordinal numbers are returned
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", as.roman(1:100), "(?!\\w)"),
                          as.numeric(1:100), safe = TRUE, perl = TRUE)
  ords <- english::ordinal(1:100)
  ords <- paste0(ords,
                 dplyr::if_else(stringr::str_count(ords, "\\S+") == 2,
                         paste0("|", gsub(" ", "-", as.character(ords))), ""))
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", ords, "(?!\\w)"),
                          as.numeric(1:100), safe = TRUE, perl = TRUE,
                          ignore.case = TRUE, fixed = FALSE)
  num <- english::words(1:100)
  num <- paste0(num,
                 dplyr::if_else(stringr::str_count(num, "\\S+") == 2,
                                paste0("|", gsub(" ", "-",
                                                 as.character(num))), ""))
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", num, "(?!\\w)"),
                          as.numeric(1:100), safe = TRUE, perl = TRUE,
                          ignore.case = TRUE, fixed = FALSE)
  # Step four: make sure most punctuations extra whitespaces are removed
  out <- gsub("(?!\\-|\\(|\\))[[:punct:]]", "", out, perl = TRUE)
  # removes all punctuations but hyphen and parentheses,
  # which may contain important information for distinguishing
  # treaties
  out <- stringr::str_squish(out)
  out
}

#' @rdname standardise_titles
#' @export
standardize_titles <- standardise_titles

# Helper functions
correct_words <- function(s) {
  s <- purrr::map(s, as.character)
  # If no arguments, the list of corrected words appears
  if (missing(s)) {
    corrected_words <- as.data.frame(corrected_words)
    corrected_words <- knitr::kable(corrected_words, "simple")
    corrected_words
  } else {
    # Substitute matching words for corrected words
    corrected_words <- as.data.frame(corrected_words)
    for (k in seq_len(nrow(corrected_words))) {
      s <- gsub(paste0(corrected_words$words[[k]]),
                paste0(corrected_words$corr_words[[k]]),
                s, ignore.case = TRUE, perl = T)
      }
    s
  }
}

corrected_words <- dplyr::tribble(~words,~corr_words,
"U\\.K\\.|^UK$","UK",
"U\\.S\\. |^US$","USA",
"U.S.S.R.","USSR",
"Art[[:punct:]]","Article",
"Co-operation|Coperation","Cooperation",
"Co-operative|Coperative","Cooperative",
"Wild Life|Wild-Life","Wildlife",
"Decision Making","Decision-Making",
"MaasMeuse","Maas",
"Test-Ban","Test Ban",
"Foot-and-Mouth","Foot and Mouth",
"Nuclear-Weapon-Free","Nuclear Weapon Free",
"Public Participation","Public Participation",
"Deep-Sea","Deep Sea",
"Land-Based|Landbased","Land Based",
"Vietnam","Viet Nam",
"Shipsballast|Ship's ballast","Ships Ballast",
"North-East|Northeast","North East",
"North-Eastern|Northeastern","North Eastern",
"North-West|Northwest","North West",
"North-western|Northwestern","North Western",
"South-East|Southeast","South East",
"South-Eastern|Southeastern","South Eastern",
"South-West|Southwest","South West",
"South-Western|Southwestern","South Western",
"Indo-Pacific|Indopacific|Asia-Pacific|Asiapacific","Asia Pacific",
"Co-ordinating|cordinating","Coordinating"
)
