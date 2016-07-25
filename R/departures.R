
SERVICE_AVT <- "avt"
PARAM_STATION <- "station"

# http://www.ns.nl/en/travel-information/ns-api/documentation-up-to-date-departure-times.html
#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
departures <- function(id) {

  path <- makepath(SERVICE_AVT)
  query <- list(station=id)
  ns(path, query)
}
