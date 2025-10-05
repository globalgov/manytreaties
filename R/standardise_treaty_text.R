#' Standardise treaty texts
#'
#' The function standardises treaty texts by removing punctuation and markers,
#' while splitting these texts into articles and annexes.
#' @name standardise_treaty_text
#' @param textvar A text variable.
#' @importFrom purrr map
#' @importFrom stringi stri_trans_general
#' @importFrom tm stripWhitespace
#' @return A list of treaty sections of the same length.
#' @details Treaty texts are not always similar when imported to R.
#' Some treaty texts, for example, contain paragraph markers while others
#' come in one text chunk.
#' `standardise_treaty_text()` facilitates the cleaning and annotation of
#' these treaty texts so that information about clauses can be retrieved
#' at a later stage with the `retrieve_clauses()` function.
#' @examples
#' \dontrun{
#' standardise_treaty_text(sample(manyenviron::agreements$HUGGO$MainText, 30))
#' }
#' @export
standardise_treaty_text <- function(textvar) {
  t <- purrr::map(textvar, function(x) {
    x <- unlist(x)
    x <- stringi::stri_trans_general(tolower(as.character(x)),
                                     id = "Latin-ASCII")
    x <- stringi::stri_replace_all_regex(x, "\nannex| \nannex|\n annex| \n annex|
                                  |\\.\\sannex\\s|\\.annex\\s|
                                  |\\d\\sannex\\s", ". ANNEX ")
    x <- stringi::stri_replace_all_regex(x, "\narticle|\n article|\nart\\.|\n art\\.|
                                  |\\.\\sarticle\\s|\\.article\\s|\nchapter|
                                  |\n chapter|\\.\\schapter\\s|\\.chapter\\s|
                                  | \narticle| \n article",
                                  ". ARTICLE ")
    x <- stringi::stri_replace_all_regex(x, "\nappendix|\n appendix|\\.\\sappendix\\s|
                                  |\\.appendix\\s| \nappendix| \n appendix",
                                  ". APPENDIX ")
    x <- stringi::stri_replace_all_regex(x, "\nprotocol|\n protocol|\\.\\sprotocol\\s|
                                  |\\.protocol\\s|\\d\\sprotocol\\s|
                                  | \nprotocol| \n protocol",
                                  ". PROTOCOL ")
    x <- stringi::stri_replace_all_regex(x, "(http\\:\\/\\/)(.+)(?=\\s)", "")
    x <- stringi::stri_replace_all_regex(x, "(?<=\\<)(.+)(?=\\>)", "")
    x <- stringi::stri_replace_all_regex(x, "\r|\t|\n", "")
    x <- stringi::stri_replace_all_regex(x, "\\<|\\>|\\-\\-", "")
    x <- stringi::stri_replace_all_regex(x, "this page uses javascript|this website uses javascript|
                                    |javascript is required (for this page)?|javascript|java script|
                                    |please use a javascript enabled browser", "")
    x <- tm::stripWhitespace(x)
    x
  })
  t <- ifelse(lengths(t) == 0, NA_character_, t)
  t
}

#' @rdname standardise_treaty_text
#' @export
standardize_treaty_text <- standardise_treaty_text
