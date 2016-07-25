# http://www.ns.nl/en/travel-information/ns-api/documentation-up-to-date-departure-times.html
#' Title
#'
#' @param id The code (abbreviation) or short name or medium-length name or full name or synonym of the station's name
#'
#' @return
#' @export
#'
#' @examples
ns_departures <- function(id) {

  if (is.null(id)) {
    stop("No station code, name or synonym provided", call. = FALSE)
  }

  # get the list of stations
  path <- makepath("avt")
  query <- list(station = id)
  res <- ns(path, query)

  # turn xml into a dataframe
  df <- xmlToDataFrame(res$content)[c("RitNummer",
                                      "VertrekTijd",
#                                      "VertrekVertraging",
#                                      "VertrekVertragingTekst",
                                      "EindBestemming",
                                      "TreinSoort",
#                                      "RouteTekst",
                                      "Vervoerder",
                                      "VertrekSpoor"
#                                      "ReisTip",
#                                      "Comments"
                                      )]

  # TODO: gewijzigd vertrekspoor?
  # TODO: optional elements

  # flatten the full and abbreviated station names
  #nameattr <- c("Kort", "Middel", "Lang")
  #df <- cbind(df, sapply(nameattr, function(x) {
  #  apply(df, 1, function(y) {
  #    xmlValue(getNodeSet(res$content, paste0("//Station[Code=\"", y["Code"], "\"]/Namen/", x))[[1]])
  #  })
  #}))

  # correct column casing
  names(df) <- tolower(names(df))

  df
}
