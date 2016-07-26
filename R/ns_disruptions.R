#' Disruptions
#'
#' @param id The code (abbreviation) or short name or medium-length name or full name or synonym of the station's name
#' @param actualBoolean (true or false) indicator of the current disruptions must be returned. This includes both unscheduled disruptions at the moment of the request, as well as engineering work scheduled to take place within two hours of the request. This parameter will provide the information described above in section 1.
#' @param unplanned Boolean (true or false) indicator of the scheduled engineering work for the next two weeks must be returned. This parameter provides the information described above in section 2. Note: unplanned=true will return the scheduled engineering work. This is therefore the opposite of what the parameter name would imply.
#'
#' @return
#' @export
#'
#' @examples
ns_disruptions <- function(id, actual = TRUE, unplanned = TRUE) {

  #if (is.null(id)) {
  #  stop("No station code, name or synonym provided", call. = FALSE)
  #}

  # get the list of stations
  path <- makepath("storingen")
  query <- list(station = id, actual = actual, unplanned = unplanned)
  res <- ns(path, query)

  # turn xml into a dataframe
  dfg <- xmlToDataFrame(nodes = getNodeSet(res$content, "//Storingen/Gepland/Storing"))[c("id",
                                      "Traject",
                                      "Periode",
                                      "Advies",
                                      "Bericht",
                                      "Oorzaak",
                                      "Vertraging"
  )]
  dfo <- xmlToDataFrame(nodes = getNodeSet(res$content, "//Storingen/Ongepland/Storing"))[c("id",
                                                                                         "Traject",
                                                                                         "Reden",
                                                                                         "Bericht",
                                                                                         "Datum"
  )]

  # correct column casing
  names(dfg) <- tolower(names(dfg))
  names(dfo) <- tolower(names(dfo))

  dfg
}
