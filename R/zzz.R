.onAttach <- function(lib, pkg) {
  msg <- c(paste0("manytreaties ", utils::packageVersion("manytreaties")),
           "\nFor more information about many packages please visit https://manydata.ch/",
           "\nType 'citation(\"manytreaties\")' for citing this R package in publications.")
  packageStartupMessage(msg)      
  invisible()
}
