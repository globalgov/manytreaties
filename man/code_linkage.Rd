% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_agreements.R
\name{code_linkage}
\alias{code_linkage}
\title{Code Agreement Linkages}
\usage{
code_linkage(title, date, return_all = FALSE)
}
\arguments{
\item{title}{A character vector of treaty title}

\item{date}{A date variable}

\item{return_all}{Do you want all the variables to be returned in a list?
By default, FALSE.}
}
\value{
A character vector of the agreements that are linked
}
\description{
Identify the linkage between amendments and protocols to a main agreement.
}
\details{
The function identifies duplicates by excluding
"predictable" words from strings, this maintains key words then used
to identify and link duplicates.
This is a choice that considers errors should lie on the side of false
negatives rather than false positives.
For the complete list of words removed from title to identify duplicates
please run the function without arguments (i.e. \code{code_linkage()}).
}
\examples{
\dontrun{
IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
code_linkage(IEADB$Title, IEADB$Begin)
}
}
