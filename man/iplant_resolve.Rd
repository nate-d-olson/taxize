% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iplant_resolve.R
\name{iplant_resolve}
\alias{iplant_resolve}
\title{iPlant name resolution}
\usage{
iplant_resolve(query, retrieve = "all", ...)
}
\arguments{
\item{query}{Vector of one or more taxonomic names. (no common names)}

\item{retrieve}{Specifies whether to retrieve all matches for the names submitted. One of 'best'
(retrieves only the single best match for each name submitted) or 'all' (retrieves all matches)}

\item{...}{Curl options passed on to \code{\link[httr]{GET}}}
}
\value{
A data.frame
}
\description{
iPlant name resolution
}
\examples{
\dontrun{
iplant_resolve(query=c("Helianthus annuus", "Homo sapiens"))
iplant_resolve("Helianthusss")
iplant_resolve("Pooa")

library("httr")
iplant_resolve("Helianthusss", config=verbose())
}
}

