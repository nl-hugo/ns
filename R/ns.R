#' @rdname ns
#' @export
#'
#' @title
#'    Connect to NS API and return information about planned and current time tables.
#'
#' @description
#' NS requires users to register to access the API. Upon registration,
#' https://www.ns.nl/ews-aanvraagformulier/?0
#'    This is a description
#'
#'
#' @seealso
#'    \link{http://www.ns.nl/en/travel-information/ns-api}
#'

#' The URL to the webservices API
BASE_URL <- "https://webservices.ns.nl"

#' The API path
API_URL <- "ns-api"

#' The user agent
UA <- user_agent("http://github.com/nl-hugo/ns")


#' Retrieves NS API user credentials.
#'
#' The user's API credentials are retrieved from the \strong{\code{NS_ID}} and
#' \strong{\code{NS_PW}} keys in the local .Renviron file. An error will occur
#' if either one or both of the keys are missing.
#'
#' @return a \code{httr::authenticate} object to be used as HTTP header for
#'   authentication
#'
#' @seealso
#'   \code{\link[httr]{authenticate}}
#'
auth <- function() {

  id <- Sys.getenv("NS_ID")
  pw <- Sys.getenv("NS_PW")
  if (identical(id, "") || identical(id, "")) {
    stop("Please set env var NS_ID and NS_PW to your personal NS API user id
         and password",
         call. = FALSE)
  }
  httr::authenticate(id, pw, type = "basic")
}


#' Returns the path to the API URL for the requested service.
#'
#' @param service The name of the service
#'
#' @return The path to the API service.
#'
#' @examples
#' makepath("stations-v2")
makepath <- function(service) {

  paste(API_URL, service, sep = "-")
}


#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
ns <- function(path, query = NULL) {

  # build the request URL
  url <- httr::modify_url(BASE_URL, path = path, query = query)

  # make request
  resp <- httr::GET(url,
              auth(),
              UA,
              content_type_json(),
              accept_xml()
              )

  # check response type
  if (httr::http_type(resp) != "text/xml") {
    stop("NS API did not return xml", call. = FALSE)
  }

  # parse response
  parsed <- xmlParse(httr::content(resp,
                             as = "text",
                             type = "text/xml",
                             encoding = "UTF-8"
                             ))

  # stop on http error
  if (http_error(resp)) {
    stop(
      sprintf(
        "NS API request failed [%s]\n%s",
        status_code(resp),
        xmlValue(getNodeSet(parsed, "//faultstring/text()")[[1]])
      ),
      call. = FALSE
    )
  }

  # stop on error message
  err <- getNodeSet(parsed, "//error")
  if (length(err) > 0) {
    parsed = NULL
    warning(
      sprintf(
        "NS API returned an error message\n%s",
        xmlValue(getNodeSet(err[[1]], "//message/text()")[[1]])
      ),
      call. = FALSE
    )
  }

  # return S3 object
  invisible(
    structure(
      list(
        content = parsed,
        path = path,
        response = resp
      ),
      class = "ns"
    )
  )
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.ns <- function(x, ...) {

  cat("<NS ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}
