
SERVICE_STORINGEN <- "storingen"

path <- paste("ns-api", SERVICE_STORINGEN, sep = "-")

# http://www.ns.nl/en/travel-information/ns-api/documentation-disruptions-and-maintenance-work.html
requestDisruptions <- function(id) {
  query <- list(station=id)
  ns(path, query)
}
