% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ns_stations.R
\name{ns_stations}
\alias{ns_stations}
\title{Returns a list of stations.}
\usage{
ns_stations()
}
\value{
A dataframe containing the stations.

This function implements \strong{version 2} of the \href{http://www.ns.nl/en/travel-information/ns-api/documentation-station-list.html}{Station list}
service of the NS API. It allows the user to request the details of all
stations. The following details are returned:

\describe{
  \item{code}{Station identification abbreviation.}
  \item{type}{Station type.}
  \item{land}{Station country code:}
  \item{uiccode}{Station UIC code, identification code of the international railway union, Union Internationale des Chemins de fer.}
  \item{lat}{Station latitude coordinate.}
  \item{lon}{Station longitude coordinate.}
  \item{kort}{Brief name of the station, consisting of max.10  characters.}
  \item{middel}{Medium-length name of the station, consisting of max. 16 characters.}
  \item{land}{Full name of the station, consisting of max. 25 characters.}
}

Please note that this implementation returns the 'namen' element as
flattened columns and that the element 'synoniemen' is not returned in the
dataframe. It can be obtained from the XML however.
}
\description{
Returns a list of stations.
}
\examples{
ns_stations()
}
\seealso{
\url{http://www.ns.nl/en/travel-information/ns-api/documentation-station-list.html}
}

