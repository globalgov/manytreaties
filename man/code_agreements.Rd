% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_agreements.R
\name{code_agreements}
\alias{code_agreements}
\title{Code Agreement Titles}
\usage{
code_agreements(dataset = NULL, title, date)
}
\arguments{
\item{dataset}{A 'many' dataset.
If provided without a title and date variables, the function finds title and
date conforming columns in the dataset.
The function "expects" that there are variables named \code{Title} and \code{Begin}
that they have been standardised using \code{standardise_titles()} and
\code{messydates::as_messydate()}, respectively.}

\item{title}{A title variable.
The function "expects" that the variable has been standardised using
\code{standardise_titles()}.}

\item{date}{A date variable.
The function "expects" that the variable has been standardised using
\code{messydates::as_messydate()}.}
}
\value{
a character vector with the treatyIDs
}
\description{
Creates an ID that contains information on the parties to an agreement,
the type of agreement, the date, and the linkage to other agreements.
}
\examples{
\dontrun{
IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
code_agreements(dataset = IEADB)
code_agreements(title = IEADB$Title, date = IEADB$Begin)
}
}
