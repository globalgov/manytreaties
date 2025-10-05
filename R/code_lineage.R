#' Code lineage from agreement titles
#'
#' @param title A title column for agreements
#' @param datacube A datacube from the many packages ecosystem.
#' @return A list of lineages that combines agreement area
#' and agreement action.
#' @importFrom purrr map
#' @importFrom stringi stri_trans_general
#' @importFrom manystates code_states
#' @examples
#' \dontrun{
#' code_lineage(title = sample(manyenviron::agreements$IEADB$Title, 30))
#' code_lineage(datacube = manyenviron::agreements)
#' }
#' @export
code_lineage <- function(title = NULL, datacube = NULL) {
  if (is.null(title) & is.null(datacube)) {
    stop("Please declare a title column or a many datacube")
  }
  # Get title variable from datacube, if available
  if (is.null(title)) {
    title <- unname(unlist(purrr::map(datacube, "Title")))
    vars <- unlist(purrr::map(datacube, names))
    if (any("Text" == vars)) { # Find text variable in datacube, if available
      txt <- unname(unlist(purrr::map(datacube, "Text")))
      txt <- read_clauses(standardise_treaty_text(txt), "preamble")
    }
  }
  # code entity and actions for titles
  title <- stringi::stri_trans_general(title, id = "Latin-ASCII")
  entity <- code_entity(title)
  domain <- code_domain(title)
  parties <- manystates::code_states(title)
  # Get entity and actions from preamble if missing from title
  if (exists("txt")) {
    entity <- ifelse(is.na(entity), code_entity(txt), entity)
    domain <- ifelse(is.na(domain), code_domain(txt), domain)
  }
  # Paste all together
  lineage <- ifelse(is.na(entity), paste0(parties, " - ", domain),
                    paste0(entity, " - ", domain))
  lineage <- gsub("- NA|NULL", "", lineage)
  lineage <- trimws(gsub("^-", "", lineage))
  lineage
}

#' Code Agreement Entity
#'
#' @param title Treaty titles
#' @return The region of the agreement
#' @examples
#' \dontrun{
#' title <- sample(manyenviron::agreements$IEADB$Title, 30)
#' code_entity(title)
#' }
#' @export
code_entity <- function(title) {
  # Add a note about JavaScript
  cli::cli_alert_info("Please make sure JavaScript is installed.")
  # Download entity package
  pkgs <- NULL
  pkgs <- data.frame(utils::installed.packages())
  if (!any(grepl("entity", pkgs$Package))) {
    remotes::install_github("trinker/entity")
    cli::cli_alert_info("Downloaded entity package.")
  }
  # Make sure necessary model is available (adapted from entity package)
  outcome <- "openNLPmodels.en" %in% list.files(.libPaths())
  if (!outcome) {
    utils::install.packages(
      "http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",
      repos = NULL,
      type = "source")
  }
  suppressWarnings(requireNamespace("entity", quietly = TRUE))
  # Code entity
  out <- suppressWarnings(entity::location_entity(title))
  # Code entity (using spacy for better results)
  # # Add a note about python
  # usethis::ui_info("Please make sure spacyr, minicinda, python, and spacy are installed.
  #                   This can be done by running 'spacyr::spacy_install()'")
  # spacyr::spacy_initialize()
  # out <- spacyr::entity_extract(spacyr::spacy_parse(title, entity = TRUE),
  #                       type = "named")
  #   dplyr::filter()
  #   dplyr::group_by(doc_id) %>%
  #   dplyr::summarise(entity_type = paste(entity_type, collapse = ", "),
  #                    entity = paste(gsub("_", " ", entity), collapse = ", "))
  # title <- data.frame(title)
  # title$doc_id <- paste0("text", as.numeric(rownames(title)))
  # out <- dplyr::left_join(title, out, by = "doc_id")
  # Remove states
  parties <- paste(manystates::code_states()$Label, collapse = "|")
  out <- gsub(parties, "", out, ignore.case = TRUE)
  out <- gsub("^c|Britain|England", "", out)
  out <- gsub("[^[:alnum:]]", " ", out)
  out <- stri_squish(out)
  out <- gsub("NULL", NA_character_, out)
  out <- ifelse(grepl("^$", out), NA_character_, out)
  out
}

#' Code domains from agreement titles
#'
#' @param title Treaty titles
#' @param type Issue-type of agreement, either environment or health
#' @return The domain taken from agreement title
#' @importFrom dplyr case_when
#' @examples
#' \dontrun{
#' title <- sample(manyenviron::agreements$IEADB$Title, 30)
#' code_domain(title)
#' }
#' @export
code_domain <- function(title, type = c("environment", "health")) {
  if(type == "environment") {
    domain <- dplyr::case_when(
      # For environmental treaties
      grepl("\\<biodiversity\\>|\\<species\\>|\\<habitat\\>|
          |\\<ecosystems\\>|biological diversity|environment|
          |genetic resources|\\<biosphere\\>|forest|\\<tree\\>|plant",
            title, ignore.case = T) ~  "environment",
      grepl("\\<air\\>|atmos|\\<climate\\>|outer space|rising temperature|
          |\\<ozone\\>|\\<emissions\\>|\\<coal\\>|global warming|
          |clean energy|renewable",
            title, ignore.case = T) ~ "climate change",
      grepl("\\<legal\\>|\\<enforcement\\>|\\<policy\\>|
          |\\<planning\\>|\\<institution\\>|\\<dispute\\>|
          |\\<court\\>|\\<tribunal\\>|\\<law\\>|settlement",
            title, ignore.case = T) ~ "law enforcement",
      grepl("\\<energy\\>|nuclear power|nuclear energy|\\<oil\\>|\\<mining\\>|
          |\\<gas\\>|hydro|\\<power\\>|generator|transmission lines",
            title, ignore.case = T) ~  "energy",
      grepl("agricultur|\\<food\\>|\\<livestock\\>|\\<crop\\>|
          |\\<crops\\>|\\<irrigation\\>|\\<cattle\\>|productivity|
          |\\<meat\\>|\\<farm\\>|\\<cultivate\\>|\\<poultry\\>|pesticide",
            title, ignore.case = T) ~  "agriculture",
      grepl("\\<waste\\>|pollut|\\<noise\\>|\\<toxic\\>|\\<hazard\\>|
          |chemical|\\<mercury\\>|residual|corrosive|substances",
            title, ignore.case = T) ~  "waste",
      grepl("\\<culture\\>|scien|techno|\\<trade\\>|\\<research\\>|
          |\\<knowledge\\>|\\<data\\>|\\<information\\>|survey",
            title, ignore.case = T) ~  "research",
      grepl("weapon|\\<military\\>|\\<proliferation\\>|
          |\\<police\\>|\\<security\\>|\\<terrorism\\>|
          |nuclear material|defense|mutual defense>|mass destruction",
            title, ignore.case = T) ~  "defense",
      grepl("alliance|peace|\\<friendship\\>|\\<allied\\>|
          |non-agression|non agression", title, ignore.case = T) ~  "peace",
      grepl("fish|\\<salmon\\>|\\<herring\\>|\\<tuna\\>|
          |\\<aquaculture\\>|\\<mariculture\\>|
          |\\<molluscs\\>|whaling", title, ignore.case = T) ~  "fishing",
      grepl("financ|\\<fund\\>|\\<funding\\>|\\<loan\\>|\\<lease\\>|
          |\\<debt\\>", title, ignore.case = T) ~  "finance",
      grepl("\\<trade\\>|\\<tariff\\>|\\<tax\\>|\\<exchange\\>|
          |\\<business\\>|economic union|economic community|free trade|
          |common market|economic partnership|
          |economic cooperation|economic zone|
          \\<invest\\>|\\<BIT\\>|\\<BITs\\>|\\<Tips\\>",
            title, ignore.case = T) ~ "trade",
      grepl("human rights|refugee|\\<genocide\\>|\\<discrimination\\>|
          |cultural rights|political rights|\\<torture\\>|\\<indigenous\\>|
          |\\<migrants\\>|\\<disabilities\\>|\\<stateless\\>|
          |geneva convention|\\<mines\\>|migration|peoples",
            title, ignore.case = T) ~ "human rights",
      grepl("\\<health\\>|disease|\\<tobacco\\>|\\<asbestos\\>|
          |\\<nursing\\>|\\<cancer\\>|\\<COVID\\>|hospital|poison|
          |\\<radiation\\>|\\<accidents\\>|medicine|medical|
          medication", title, ignore.case = T) ~ "health",
      grepl("boundar|\\<territorial\\>|delimit|
          |frontier|\\<border\\>|\\<limit\\>|\\<limits\\>",
            title, ignore.case = T) ~ "territorial boundaries",
      grepl("outer space|\\<moon\\>|\\<satellite\\>|space liability|
          |space station|mars", title, ignore.case = T) ~ "outer space")
    domain
  } else{
    domain <- dplyr::case_when(
      # For health treaties
      # Labour (ILO), Human Rights (Rights, Refugees, Euthanasia, Ethical Standards, Tehran Declaration)
      # Protection (Prisoners, Juvenile Justice, Women, Disabilities, Minorities, Older Persons/Ageing, Social Charter, Social and Medical Assistance)
      # Mental Health (Caracas Declaration), Prevention (Alcohol/Drugs/Tobacco, Diseases)
      # Healthcare (Medical care, Data files), Pollution (Waste, pollution, transboundary), Climate change (Environment)
      grepl("\\<ILO\\>|\\<labour\\>",
            title, ignore.case = T) ~  "labour",
      grepl("right|\\<refugees\\>|\\<euthanasia\\>|\\<ethical\\>|
          |tehran declaration|genocide|nuremberg|teheran",
            title, ignore.case = T) ~ "human rights",
      grepl("\\<protection\\>|prisoner|\\<juvenile\\>|beijing|
          |\\<child\\>|\\<women\\>|\\<disabilities\\>|disappearance|
          |\\<minorities\\>|\\<ageing\\>|older persons|female|
          |social charter|\\<assistance\\>|armed|geneva|\\<mines\\>|
          |torture|humane|\\<oas\\>|discrimination|traffic|conduct",
            title, ignore.case = T) ~ "protection",
      grepl("\\<alcohol\\>|\\<drugs\\>|\\<narcotic\\>|\\<tobacco\\>",
            title, ignore.case = T) ~  "prevention",
      grepl("\\<mental\\>|caracas declaration|mental health|
          |\\<psychosocial\\>",
            title, ignore.case = T) ~  "mental health",
      grepl("\\<waste\\>|pollut|\\<noise\\>|\\<toxic\\>|\\<hazard\\>|nuclear|
          |chemical|\\<mercury\\>|residual|corrosive|substances|transboundary",
            title, ignore.case = T) ~  "pollution",
      grepl("\\<medical\\>|data files|world health organization|alma ata|
          |population|",
            title, ignore.case = T) ~  "healthcare",
      grepl("climate|\\<rio\\>|\\<habitat\\>|\\<environment\\>|agenda 21",
            title, ignore.case = T) ~  "climate change")
    domain
  }
}

#' Code areas from agreement titles
#'
#' @param title Treaty titles
#' @param return Whether to return the full area name or the short version.
#'   "full" by default.
#' @return The area taken from agreement title
#' @examples
#' \dontrun{
#' code_area(HUGGO$Title)
#' }
#' @export
code_area <- function(title, return = c("full", "short")) {
  
  return <- match.arg(return)
  match_to_table(title, area_type, "terms", return)

}

# Area type data ####
area_type <- dplyr::tribble(~short,~full,~terms,
                            "Adriatic","Adriatic Sea","Adriatic",
                            "Alberta","Lake Alberta","Lake Alberta",
                            "Amudarya","Amudarya River","Amudarya|Amu-Darya|Oxus",
                            "Amur","Amur River","Heilong|Amur",
                            "Andaman","Andaman Sea","Andaman Sea",
                            "Aral","Aral Sea","Aral Sea",
                            "Araxes","Araxes River","Araxes|Aras",
                            "Atrak","Atrak River","Atrek|Atrak",
                            "Azov","Azov Sea","Azov Sea|Sea Of Azov",
                            "Baltic","Baltic Sea","Baltic|Baltic Sea",
                            "Barents","Barents Sea","Barents",
                            "Beaufort","Beaufort Sea","Beaufort",
                            "Bering","Bering Sea","Bering",
                            "Bidassoa","Bidassoa River","Bidassoa",
                            "Biscay","Bay of Biscay","Bay Of Biscay",
                            "Black","Black Sea","Black Sea",
                            "Brahmaputra","Brahmaputra River","Brahmaputra",
                            "Caledon","Caledon River","Caledon River",
                            "Caribbean","Caribbean Sea","Caribbean",
                            "Caspian","Caspian Sea","Caspian",
                            "Chad","Lake Chad","Lake Chad",
                            "Colorado","Colorado River","Colorado River",
                            "Columbia","Columbia River","Columbia River",
                            "Congo","Congo River","Congo River|Zaire River",
                            "Constance","Lake Constance","Constance|Bodensee",
                            "Corubal","Corubal River","Koliba-Korubal|Corubal",
                            "Cuareim","Cuareim River","Cuareim",
                            "Danube","Danube River","Danube|Donau|Rajka-Gonyu",
                            "Dniester","Dniestr River","Dniester|Dniestr|Dnestr",
                            "Dojran","Lake Dojran","Dojran|Lake Ohrid|Doiran",
                            "Douro","Douro River","Douro",
                            "Dover","Straints of Dover","Straits Of Dover",
                            "Elefantes","Elefantes River","Elefantes|Massingir",
                            "Enningdal","Enningdal River","Enningdal|Enningdalselva",
                            "Euphrates","Euphrates River","Euphrates|Al-Asi",
                            "Finland","Gulf of Finland","Gulf Of Finland",
                            "Fraser","Fraser River","Fraser",
                            "Gambia","Gambia River","Gambia River|River Gambia",
                            "Gander","Gander River","Gander",
                            "Garda","Lake Garda","Garda",
                            "Gash","Gash River","River Gash|Khor Baraka|Gash River",
                            "Geneva","Lake Geneva","Lake Geneva|Lac Leman|Leman Lake",
                            "Grande","Rio Grande","Rio Grande",
                            "GreatLakes","Great Lakes","Great Lakes",
                            "GreatBarrier","Great Barrier Reef","Great Barrier Reef",
                            "Hanka","Lake Hanka","Hanka|Khanka",
                            "Helmand","Helmand River","Helmand|Hilmand",
                            "Horgos","Khorgos River","Khorgos|Korgas|Horgos",
                            "Iddefjord","Iddefjord","Iddefjord|Iddefjorden",
                            "Inari","Lake Inari","Inari",
                            "Indus","Indus River","Indus",
                            "Jaguarao","Jaguarao River","Jaguarao",
                            "Kagera","Kagera River","Kagera",
                            "Khorgos","Khorgos River","Khorgos",
                            "Kolente","Kolente River","Kolente|Great Scarcies",
                            "Komati","Komati River","Komati",
                            "Kunene","Kunene River","Kunene|Cunene",
                            "Kwando","Kwando River","Kwando|Linyanti",
                            "Ladoga","Lake Ladoga","Ladoga",
                            "Lanoux","Lanoux River","Lanoux|Lanos",
                            "Latorica","Latorica River","Latorica|Latoritza",
                            "Lielupe","Lielupe River","Lielupe|Western Dvina",
                            "Litani","Litani River","Litani|Leontes",
                            "Lugano","Lake Lugano","Lugano|Luganus",
                            "Lys","Lys River","Lys|Leie",
                            "Mahakali","Mahakali River","Karnali|Mahakali",
                            "Mano","Mano River","Mano River|Manya",
                            "Maule","Maule River","Maule",
                            "Mediterranean","Mediterranean Sea","Mediterranean",
                            "Mekong","Mekong River","Mekong",
                            "Mekrou","Mekrou River","Mekrou",
                            "Memphremagog","Lake Memphremagog","Memphremagog",
                            "Meuse","Meuse River","Maas|Meuse",
                            "Minho","Minho River","Minho|Mino",
                            "Naaf","Naaf River","Naaf|Naf",
                            "Neiden","Neiden River","Neiden|Neidenelva|Naatamo",
                            "Neman","Neman River","Neman|Nemunas|Memel",
                            "Nestos","Nestos River","Nestos|Nestus",
                            "Niagara","Niagara River","Niagara",
                            "Niger","Niger River","Niger River",
                            "Nile","Nile River","Nile",
                            "North","North Sea","North Sea",
                            "Oder","Oder River","Oder|Odra",
                            "Okavango","Okavango River","Okavango",
                            "Okhotsk","Sea of Okhotsk","Okhotsk|Sea Of Okhotsk",
                            "Orange","Orange River","Orange River|Orange-Senqu",
                            "Orawa","Orawa River","Orawa|Orava",
                            "Pacific","Pacific Ocean","Pacific Ocean",
                            "Paraguay","Paraguay River","Paraguay River|Rio Paraguay|River Paraguay",
                            "Parana","Parana River","Parana",
                            "Pasvik","Pasvik River","Pasvik|Paz River",
                            "Peipsi","Lake Peipsi","Peipsi|Lake Peipus",
                            "Pilcomayo","Pilcomayo River","Pilcomayo",
                            "Plate","La Plata River","La Plata|River Plate|Plate River",
                            "Prut","Prut River","Prut|Pruth",
                            "Rainy","Rainy Lake","Rainy Lake|Lake Rainy",
                            "RedSea","Red Sea","Red Sea",
                            "RedRiver","Red River","Red River",
                            "Rhine","Rhine River","Rhine",
                            "Ross","Ross Sea","Ross Sea",
                            "Roya","Roya River","Roya River|River Roya",
                            "Saimaa","Lake Saimaa","Saima",
                            "Sava","Sava River","Sava",
                            "Scheldt","Scheldt River","Scheldt",
                            "Selenga","Selenga River","Selenga|Selenge",
                            "Senegal","Senegal River","Senegal River",
                            "Skagit","Skagit River","Skagit",
                            "Skagerrak","Skagerrak Strait","Skagerrak|Skagerak",
                            "Souris","Souris River","Souris",
                            "StFrancis","Lake St Francis","Lake St Francis|Lake Saint Francis",
                            "StJohn","St John River","Saint John River|St John River",
                            "StLawrence","St Lawrence River","Saint Lawrence River|St Lawrence River",
                            "Talas","Talas River","Talas",
                            "Tana","Tana River","Tana|Tana River",
                            "Tanganyika","Lake Tanganyika","Tanganyika",
                            "Teesta","Teesta River","Teesta|Tista",
                            "Tigris","Tigris River","Tigris",
                            "Timok","Timok River","Timok|Timoc",
                            "Timor","Timor Sea","Timor Sea",
                            "Tisza","Tisza River","Tisza|Theiss",
                            "Titicaca","Lake Titicaca","Titicaca",
                            "Tonkin","Tonkin Gulf","Tonkin|Gulf Of Tonkin",
                            "Tonle","Tonle Lake","Tonle Sap|Great Lake Of Cambodia",
                            "Tornea","Tornea River","Tornio|Tornea|Torne",
                            "Tsana","Tsana River","Tsana|Tsani",
                            "Uruguay","Uruguay River","Uruguay River|Uruguai River|River Uruguay",
                            "Vanern","Lake Vanern","Vanern",
                            "Varanger","Varanger Fjord","Varanger Fjord|Varangerfjord",
                            "Victoria","Lake Victoria","Lake Victoria",
                            "Videa","Videa River","Videa|Vydra",
                            "Vistula","Vistula River","Vistula",
                            "Vistytis","Lake Vistytis","Vistytis|Vishtynets",
                            "Volta","Volta River","Volta",
                            "Vuoksi","Vuoksi River","Vuoksi|Voksa",
                            "Walvis","Walvis Bay","Walvis Bay|Walfisch Bay",
                            "Witka","Witka River","Witka",
                            "Woods","Lake Of The Woods","Lake Of The Woods",
                            "Yalu","Yalu River","Yalu|Amnok",
                            "Yarmuk","Yarmuk River","Yarmuk",
                            "Yukon","Yukon River","Yukon",
                            "Zambezi","Zambezi River","Zambezi|Zambesi",
                            "Border","Border","Border River|Trans-border River|Transboundary River|Frontier River|Rivers Of Common Interest|Rivers Flowing Through Their Territories|Rivers Flowing Through The Territory Of Both Countries|Rivers Flowing Through The Territories Of The Both Countries|Frontier Lakes And Rivers|Watercourses Forming Part Of The Frontier|Waters Of Rivers Crossing Their Territories",
)

match_to_table <- function(char_vec, pattern_table, pattern_col, value_col, case = TRUE) {
  # Precompile regex patterns
  patterns <- pattern_table[[pattern_col]]
  values <- pattern_table[[value_col]]
  
  # Initialize result vector
  result <- character(length(char_vec))
  matched <- logical(length(char_vec))
  
  # Iterate over the lookup table (small) and apply patterns to the whole char_vec (large)
  for (i in seq_along(patterns)) {
    hits <- stringi::stri_detect_regex(char_vec, patterns[i], 
                                       case_insensitive = case) & !matched
    result[hits] <- values[i]
    matched[hits] <- TRUE  # Prevent overwriting earlier matches
  }
  
  # Replace unmatched with NA
  result[!matched] <- NA_character_
  result
}

#' Code locations from agreement titles
#'
#' @param title Treaty titles
#' @return The location taken from agreement title
#' @examples
#' \dontrun{
#' code_location(HUGGO$Title)
#' }
#' @export
code_location <- function(title) {
  unlist(stringi::stri_extract_all_regex(title, cities))
  cli::cli_alert_success("Coded locations")
}

cities <- "Amsterdam|Ankara|Athens|Auckland|Algiers|Abu Dhabi|
  Bangkok|Beirut|Bogota|Brasilia|Beijing|Berlin|Brussels|Buenos Aires|Basel|Bamako|
  Cairo|Canberra|Caracas|Copenhagen|Courtray|  
  Dakar|Dublin|Dar es Salaam|Doha|
  Helsinki|Havana|Hong Kong|
  Istanbul|
  Jakarta|
  Kabul|Kampala|Kuala Lumpur|Kyoto|
  Lisbon|London|Lusaka|
  Madrid|Manila|Moscow|
  Nairobi|New Delhi|
  Oslo|Ottawa|
  Paris|Prague|
  Rabat|Rio de Janeiro|Rome|
  Stockholm|San Jose|Santiago|Sao Paulo|Seoul|Singapore|Stockholm|
  Tokyo|Tunis|
  Vienna|
  Warsaw|Washington DC|
  Zagreb|Zurich"
