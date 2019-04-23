#' Make a URL
#'
#' Make a valid URL for the STRING API request. The functions translates STRING
#' required URL fields into \code{modify_url} arguments.
#'
#' @param database A \code{character}. Possible values are 'string-db.org'
#' (default) or 'string.embl.de' for the STRING database or 'stitch.embl.de'
#' for the STITCH sister database.
#' @param access A \code{character}. Possible values are 'api' (default) or
#' services (not used).
#' @param format A \code{character}. Only 'tsv' is used.
#' @param request A \code{character}. Possibel values are 'resolve' (default),
#' 'abstracts', 'actions', 'interactors' or 'interactions'. A corresponding
#' request with the name 'requstList' is also available when querying for more
#' than one item.
#' @param parameters A \code{list}. Different requests require different
#' parameters. The allowed parameters are documented in individual request
#' functions.
#'
#' @return A \code{character} string of the URL.
#'
#' @examples
#' # make a resolve request for ADD
#' make_url(parameters = list(identifier = 'ADD'))
#'
#' @importFrom httr modify_url
#'
#' @export
make_url <- function(database = 'string-db.org', access = 'api', format = 'tsv',
                     request = 'resolve', parameters = NULL) {
  # check database host name
  databases <- c('string-db.org', 'string.embl.de', 'stitch.embl.de')
  if(!database %in% databases) {
    stop('Provide valid database host.')
  } else {
    hostname <- database
  }

  # check and construct path
  ## access
  accesses <- c('api', 'services')
  stopifnot(access %in% accesses)

  ## format
  formats <- c('tsv', 'json', 'tsv-no-header', 'psi-mi', 'psi-mi-tab')
  stopifnot(format %in% formats)

  ## request
  requests <- c('resolve', 'resolveList', 'abstracts', 'abstractsList',
                'interactors', 'interactorsList', 'actions', 'actionsList',
                'interactions', 'interactionsList')
  stopifnot(request %in% requests)

  path <- paste(access, format, request, sep = '/')

  # check and construct parameters
  if(is.null(parameters)) {
    stop("parameters can't be NULL.")
  } else if (!is.list(parameters)) {
    stop("parameters has to be list of named items")
  } else {
    query <- parameters
  }

  # modify the url
  url <- modify_url('',
                    scheme = 'http',
                    hostname = hostname,
                    path = path,
                    query = query)

  # return url
  return(url)
}

#' Send GET request
#'
#' Send a GET request and return the response or the error.
#'
#' @param url A \code{character} string.
#'
#' @return An object of class \code{response}.
#'
#' @importFrom httr http_error GET status_code content
#'
#' @examples
#' \dontrun{
#' # make a resolve request for ADD
#' url <- make_url(parameters = list(identifier = 'ADD'))
#'
#' # send request
#' send_request(url)
#' }
#'
#' @export
send_request <- function(url) {
  # send GET request
  resp <- GET(url)

  # if error, return status code and error message
  if(http_error(resp)) {
    stop(sprintf("API request failed with code %s and the following error/s were returnd: %s",
                 status_code(resp),
                 paste(unlist(content(resp, 'parsed'), use.names = FALSE), collapse = '. ')),
         call. = FALSE)
  }

  # return response object
  return(resp)
}

#' Format the content of response.
#'
#' Capture the API response in text format then reformat into tibble.
#'
#' @param resp An object of class \code{response}.
#' @param format A \code{character}. Only 'tsv' is used.
#'
#' @return A \code{tibble}.
#'
#' @examples
#' # load example response object
#' fl <- system.file('extdata', 'resp.rda', package = 'stringapi')
#' load(fl)
#'
#' # format content
#' format_content(resp)
#'
#' @importFrom httr parse_url has_content content
#' @importFrom readr read_tsv
#'
#' @export
format_content <- function(resp, format = 'tsv') {
  # check formate is provided; if not, extract from url
  if(is.null(format)) {
    url <- parse_url(resp$url)
    format <- strsplit(url$path, '/')[[1]][2]
  }

  # check resp object hase contents
  if(has_content(resp)) {
    # extract content
    cont <- content(resp, as = 'text')
  } else {
    # or stop
    stop("resp has no content.")
  }

  # read the data in the appropriate formate
  res <- switch(format,
                'tsv' = read_tsv(cont))

  # return res
  return(res)
}
