
SERVICE_AVT <- "avt"
PARAM_STATION <- "station"

path <- paste("ns-api", SERVICE_AVT, sep = "-")

# http://www.ns.nl/en/travel-information/ns-api/documentation-up-to-date-departure-times.html
#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
requestDepartureTimes <- function(id) {
  query <- list(station=id)
  ns(path, query)
}
