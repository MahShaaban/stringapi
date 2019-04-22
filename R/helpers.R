#' @importFrom httr modify_url
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
                'interactions', 'interactionsList', 'network', 'networkList')
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

#' @importFrom httr parse_url has_content content
#' @importFrom readr read_tsv
#' @importFrom jsonlite fromJSON
#' @export
format_content <- function(resp, format = NULL) {
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
                'tsv' = read_tsv(cont),
                'json' = fromJSON(cont, simplifyVector = FALSE))

  # return res
  return(res)
}

#' @importFrom httr http_error GET status_code content
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
