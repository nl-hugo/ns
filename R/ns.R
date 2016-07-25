#' @rdname ns
#' @export
#'
#' @title
#' Connect to NS API and return information about planned and current time tables.
#'
#' @description
#' This is a description
#'
#'
#' @seealso
#' http://www.ns.nl/en/travel-information/ns-api
#'

BASE_URL <- "https://webservices.ns.nl"
API_URL <- "ns-api"

UA <- user_agent("http://github.com/nl-hugo/ns")

#' Title
#'
#' @return
#' @export
#'
#' @examples
auth <- function() {

  id <- Sys.getenv("NS_ID")
  pw <- Sys.getenv("NS_PW")
  if (identical(id, "") || identical(id, "")) {
    stop("Please set env var NS_ID and NS_PW to your personal NS API user id and password",
         call. = FALSE)
  }
  authenticate(id, pw, type = "basic")
}



#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
ns_api <- function(path, query = NULL) {

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

  # return S3 object
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "ns_api"
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
print.ns_api <- function(x, ...) {

  cat("<NS ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}
