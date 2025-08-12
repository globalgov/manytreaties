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
  
  # 1: Capitalise first letter in words ####
  out <- standardise_caps(title)
  
  # 2: Standardise strings returned ####
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
  out <- stringi::stri_replace_all_regex(out, ",\\s*", ", ")
  # standardise some country abbreviations and specific words
  out <- correct_words(out)
  # remove "the government of"
  out <- gsub("the government of ", "", out, ignore.case = TRUE)
  
  # 3: Standardise numbers ####
  out <- standardise_numbers(out)
  
  # 4: Remove most punctuation and extra whitespaces ####
  out <- gsub("(?!\\-|\\(|\\))[[:punct:]]", "", out, perl = TRUE)
  # removes all punctuations but hyphen and parentheses,
  # which may contain important information for distinguishing treaties
  out <- stri_squish(out)
  out
}

#' @rdname standardise_titles
#' @export
standardize_titles <- standardise_titles

#' @rdname standardise_titles
#' @section standardise_caps(): 
#'  This function is used to standardise the capitalisation
#'  of words in treaty titles.
#'  It capitalises the first letter of each word
#'  while keeping the rest of the letters in lowercase.
#'  Unlike stringi or stringr solutions though, this function retains
#'  abbreviations and acronyms in uppercase.
#' @export
standardise_caps <- function(title){
  cap <- function(s) paste(toupper(substring(s, 1, 1)), {
    s <- substring(s, 2)
  }, sep = "", collapse = " ")
  cli::cli_alert_success("Standardised capitalisation in titles")
  vapply(strsplit(title, split = " "), cap, "",
         USE.NAMES = !is.null(names(title)))
}

#' @rdname standardise_titles
#' @section standardise_numbers(): 
#'   This function is used to standardise numbers in treaty titles
#'   to improve readability and facilitate string matching.
#'   It replaces written numbers with their numerical equivalents.
#' @export
standardise_numbers <- function(title){

  # Change number symbol into word
  out <- stringi::stri_replace_all_regex(out, "\\#", "Number ")

  # Roman numerals
  out <- mstringi_replace_all(title,
                          paste0("(?<!\\w)", as.roman(1:100), "(?!\\w)"),
                          as.numeric(1:100))
  # Ordinal numbers
  ords <- number_words$ordinal
  ords <- paste0(ords,
                 dplyr::if_else(stringr::str_count(ords, "\\S+") == 2,
                                paste0("|", gsub(" ", "-", as.character(ords))), ""))
  out <- mstringi_replace_all(out,
                          paste0("(?<!\\w)", ords, "(?!\\w)"),
                          as.numeric(1:100))
  # Written numbers
  num <- number_words$word
  num <- paste0(num,
                dplyr::if_else(stringr::str_count(num, "\\S+") == 2,
                               paste0("|", gsub(" ", "-",
                                                as.character(num))), ""))
  out <- mstringi_replace_all(out,
                          paste0("(?<!\\w)", num, "(?!\\w)"),
                          as.numeric(1:100))
  out
}

mstringi_replace_all <- function(text, pattern, replacement) {
  stopifnot(length(pattern) == length(replacement))
  for (i in seq_along(pattern)) {
    text <- stringi::stri_replace_all_regex(text, pattern[i], replacement[i])
  }
  text
}

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

stri_squish <- function(charvec){
  stringi::stri_trim_both(stringi::stri_replace_all_regex(charvec, "\\s+", " "))
}

# Number_words ####
number_words <- dplyr::tribble(
  ~number, ~word, ~ordinal,
  1, "one", "first",
  2, "two", "second",
  3, "three", "third",
  4, "four", "fourth",
  5, "five", "fifth",
  6, "six", "sixth",
  7, "seven", "seventh",
  8, "eight", "eighth",
  9, "nine", "ninth",
  10, "ten", "tenth",
  11, "eleven", "eleventh",
  12, "twelve", "twelfth",
  13, "thirteen", "thirteenth",
  14, "fourteen", "fourteenth",
  15, "fifteen", "fifteenth",
  16, "sixteen", "sixteenth",
  17, "seventeen", "seventeenth",
  18, "eighteen", "eighteenth",
  19, "nineteen", "nineteenth",
  20, "twenty", "twentieth",
  21, "twenty-one", "twenty-first",
  22, "twenty-two", "twenty-second",
  23, "twenty-three", "twenty-third",
  24, "twenty-four", "twenty-fourth",
  25, "twenty-five", "twenty-fifth",
  26, "twenty-six", "twenty-sixth",
  27, "twenty-seven", "twenty-seventh",
  28, "twenty-eight", "twenty-eighth",
  29, "twenty-nine", "twenty-ninth",
  30, "thirty", "thirtieth",
  31, "thirty-one", "thirty-first",
  32, "thirty-two", "thirty-second",
  33, "thirty-three", "thirty-third",
  34, "thirty-four", "thirty-fourth",
  35, "thirty-five", "thirty-fifth",
  36, "thirty-six", "thirty-sixth",
  37, "thirty-seven", "thirty-seventh",
  38, "thirty-eight", "thirty-eighth",
  39, "thirty-nine", "thirty-ninth",
  40, "forty", "fortieth",
  41, "forty-one", "forty-first",
  42, "forty-two", "forty-second",
  43, "forty-three", "forty-third",
  44, "forty-four", "forty-fourth",
  45, "forty-five", "forty-fifth",
  46, "forty-six", "forty-sixth",
  47, "forty-seven", "forty-seventh",
  48, "forty-eight", "forty-eighth",
  49, "forty-nine", "forty-ninth",
  50, "fifty", "fiftieth",
  51, "fifty-one", "fifty-first",
  52, "fifty-two", "fifty-second",
  53, "fifty-three", "fifty-third",
  54, "fifty-four", "fifty-fourth",
  55, "fifty-five", "fifty-fifth",
  56, "fifty-six", "fifty-sixth",
  57, "fifty-seven", "fifty-seventh",
  58, "fifty-eight", "fifty-eighth",
  59, "fifty-nine", "fifty-ninth",
  60, "sixty", "sixtieth",
  61, "sixty-one", "sixty-first",
  62, "sixty-two", "sixty-second",
  63, "sixty-three", "sixty-third",
  64, "sixty-four", "sixty-fourth",
  65, "sixty-five", "sixty-fifth",
  66, "sixty-six", "sixty-sixth",
  67, "sixty-seven", "sixty-seventh",
  68, "sixty-eight", "sixty-eighth",
  69, "sixty-nine", "sixty-ninth",
  70, "seventy", "seventieth",
  71, "seventy-one", "seventy-first",
  72, "seventy-two", "seventy-second",
  73, "seventy-three", "seventy-third",
  74, "seventy-four", "seventy-fourth",
  75, "seventy-five", "seventy-fifth",
  76, "seventy-six", "seventy-sixth",
  77, "seventy-seven", "seventy-seventh",
  78, "seventy-eight", "seventy-eighth",
  79, "seventy-nine", "seventy-ninth",
  80, "eighty", "eightieth",
  81, "eighty-one", "eighty-first",
  82, "eighty-two", "eighty-second",
  83, "eighty-three", "eighty-third",
  84, "eighty-four", "eighty-fourth",
  85, "eighty-five", "eighty-fifth",
  86, "eighty-six", "eighty-sixth",
  87, "eighty-seven", "eighty-seventh",
  88, "eighty-eight", "eighty-eighth",
  89, "eighty-nine", "eighty-ninth",
  90, "ninety", "ninetieth",
  91, "ninety-one", "ninety-first",
  92, "ninety-two", "ninety-second",
  93, "ninety-three", "ninety-third",
  94, "ninety-four", "ninety-fourth",
  95, "ninety-five", "ninety-fifth",
  96, "ninety-six", "ninety-sixth",
  97, "ninety-seven", "ninety-seventh",
  98, "ninety-eight", "ninety-eighth",
  99, "ninety-nine", "ninety-ninth",
  100, "hundred", "hundredth"
)
