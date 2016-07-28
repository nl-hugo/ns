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

  # get the departures
  path <- makepath("avt")
  query <- list(station = id)
  res <- ns(path, query)
  df <- res$content

  # check if the api returned content
  if (!is.null(df)) {

    # turn xml into a dataframe
    df <- xmlToDataFrame(res$content)[c("RitNummer",
                                        "VertrekTijd",
                                        "EindBestemming",
                                        "TreinSoort",
                                        "Vervoerder",
                                        "VertrekSpoor"
    )]

    # append optional elements
    elt <- c("VertrekVertraging",
             "VertrekVertragingTekst",
             "RouteTekst",
             "ReisTip",
             "Comments"
    )
    df <- cbind(df, sapply(elt, function(x) {
      apply(df, 1, function(y) {
        v <- getNodeSet(res$content, paste0("//VertrekkendeTrein[RitNummer=\"",
                                            y["RitNummer"],
                                            "\"]/",
                                            x
        ))
        if (length(v) == 1) {
          v <- xmlValue(v[[1]])
        }
        v
      })
    }))

    # append changed platform indicator
    df <- cbind(df, apply(df, 1, function(x) {
      v <- getNodeSet(res$content, paste0("//VertrekkendeTrein[RitNummer=\"",
                                          x["RitNummer"],
                                          "\"]/VertrekSpoor"
      ))
      sapply(v, function(el) xmlGetAttr(el, "wijziging"))
    })
    )
    names(df)[12] <- "GewijzigdVertrekSpoor"

    # correct column casing
    names(df) <- tolower(names(df))

    # return columns in the appropriate order
    df <- df[, c(1, 2, 7, 8, 3, 4, 9, 5, 6, 12, 10, 11)]
  }
  df
}
