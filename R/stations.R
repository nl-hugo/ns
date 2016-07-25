
SERVICE_STATIONS <- "stations-v2"

path <- paste(API_URL, SERVICE_STATIONS, sep = "-")

#' Title
#'
#' @return
#' @export
#'
#' @examples
# http://www.ns.nl/en/travel-information/ns-api/documentation-station-list.html
requestStationList <- function() {

  ns(path)

}
