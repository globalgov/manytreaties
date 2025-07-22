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

