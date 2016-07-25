
SERVICE_STATIONS <- "stations-v2"

#' Title
#'
#' @return
#' @export
#'
#' @examples
# http://www.ns.nl/en/travel-information/ns-api/documentation-station-list.html
stations <- function() {

  path <- makepath(SERVICE_STATIONS)
  ns(path)

}
