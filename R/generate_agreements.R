#' Generate agreements
#' @description Generates a list of fictional agreements or a
#'   list memberships to agreements.
#' @param n Integer number of fictional agreements to generate.
#' @param treaty_type What type of agreements would you like to generate?
#'   By default, "any" agreements.
#'   Other options include "bilaterals" or "multilaterals".
#' @return String vector of fictional agreement names and/or memberships
#'   to agreements.
#' @name generate_

#' @rdname generate_
#' @examples
#' \donttest{
#' generate_agreements(12)
#' generate_agreements(12, treaty_type = "bilaterals")
#' generate_agreements(12, treaty_type = "multilaterals")
#' }
#' @export
generate_agreements <- function(n = 10, treaty_type = "any") {
  agreements <- NULL
  partylib <- trimws(paste(rep("Between The Parties of ", n),
                           sample(manystates::code_states()$Label, n, replace = TRUE),
                           rep(" and ", n),
                           sample(manystates::code_states()$Label, n, replace = TRUE)))
  typelib <- c("Agreement", "Convention")
  subjlib <- c("On", "For", "Concerning", "Limiting",
               "Regulating", "Regarding", "Relating To",
               "Cooperation In",
               "On Cooperation In The Area Of", "On Cooperation In The Field Of")
  targetlib <- c("Hydroelectric Power Station and Dams",
                 "The Brine River", "The Talamasi River",
                 "The Protection Of Migratory Birds And Birds In Danger Of Extinction And Their Environment",
                 "An International Energy Programme",
                 "North Eastern Ocean of Peace",
                 "The Development Of Fisheries",
                 "The Protection Of The Great Border Reef",
                 "The Peaceful Uses Of Atomic Energy And Space Research",
                 "Fishing For King And Tanner Crab",
                 "Peaceful Uses Of Nuclear Energy",
                 "The Fishing Of The Pico-Scindian Herring",
                 "Certain Fishery Problems On The High Seas",
                 "Whaling",
                 "Civil Liability In The Field Of Maritime Carriage Of Nuclear Material",
                 "The Establishment Of An International Fund For Compensation For Oil Pollution Damage",
                 "Marine Pollution By Dumping From Ships And Aircraft",
                 "The Protection Of Migratory Birds And Game Mammals",
                 "Navigation And Fisheries Safety",
                 "Fisheries",
                 "Environmental Protection",
                 "The Conduct Of Fishing Operations",
                 "The Development Of Geothermal Energy",
                 "Energy Research And Development",
                 "Shrimp")
  if (treaty_type == "bilaterals") {
    out <- trimws(paste(sample(typelib, n, replace = TRUE),
                        partylib,
                        sample(subjlib, n, replace = TRUE),
                        sample(targetlib, n, replace = TRUE)))
  } else if (treaty_type == "multilaterals") {
    out <- trimws(paste(sample(typelib, n, replace = TRUE),
                        sample(subjlib, n, replace = TRUE),
                        sample(targetlib, n, replace = TRUE)))
  } else {
    bilaterals <- round(sum(grepl("between", agreements$HUGGO$Title,
                                  ignore.case = TRUE)) /
                          length(agreements$HUGGO$Title), digits = 1)
    partylib[sample(n*bilaterals)] <- ""
    out <- trimws(paste(sample(typelib, n, replace = TRUE),
                        partylib,
                        sample(subjlib, n, replace = TRUE),
                        sample(targetlib, n, replace = TRUE)))
  }
  stringi::stri_replace_all_regex(out, "\\s\\s+", " ")
}
#' 
#' #' @rdname generate_
#' #' @param list Would you like all members to an agreement be listed in the
#' #' same observation?
#' #' By default, FALSE.
#' #' If TRUE, pastes all members to an agreement together in the same observation
#' #' separating them with commas.
#' #' @importFrom dplyr %>% select rename
#' #' @importFrom plyr ddply .
#' #' @examples
#' #' \donttest{
#' #' generate_memberships(12)
#' #' generate_memberships(12, treaty_type = "bilaterals")
#' #' generate_memberships(12, treaty_type = "multilaterals")
#' #' generate_memberships(12, list = TRUE)
#' #' }
#' #' @export
#' generate_memberships <- function(n = 10, treaty_type = "any", list = FALSE) {
#'   nm <- title <- NULL
#'   membs <- manystates::code_states()$stateID
#'   g_agreements <- data.frame(title = generate_agreements(n = n, treaty_type =
#'                                                            treaty_type),
#'                              nm = sample(100, n), membs = 0)
#'   coment <- vapply(manystates::code_states()[, 3], function(x) grepl(x, g_agreements$title,
#'                                                         ignore.case = T,
#'                                                         perl = T) * 1,
#'                    FUN.VALUE = numeric(n))
#'   colnames(coment) <- manystates::code_states()[, 1]
#'   rownames(coment) <- g_agreements$title
#'   out <- unname(apply(coment, 1, function(x) paste(names(x[x == 1]),
#'                                                    collapse = ", ")))
#'   out[out == ""] <- NA
#'   for (k in seq_len(length(g_agreements$nm))) {
#'     g_agreements$membs[k] <- ifelse(grepl("Between The Parties of",
#'                                           g_agreements$title[k]), out[k],
#'                                     trimws(paste(
#'                                       sample(membs,
#'                                              as.numeric(g_agreements$nm[k])),
#'                                                  collapse = ", ")))
#'   }
#'   g_agreements <- dplyr::select(g_agreements, -nm) %>%
#'     dplyr::rename(membership = membs)
#'   if (list == FALSE) {
#'     g_agreements <- plyr::ddply(g_agreements, plyr::.(title), function(DF) {
#'       data.frame(membership = trimws(strsplit(DF$membership, ",")[[1]]))
#'     })
#'   }
#'   g_agreements
#' }
