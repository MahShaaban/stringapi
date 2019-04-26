#' Make a URL
#'
#' Make a valid URL for the STRING API request. The functions translates STRING
#' required URL fields into \code{modify_url} arguments.
#'
#' @param database A \code{character}. Possible value is 'string-db.org'.
#' @param access A \code{character}. Possible values are 'api' (default) or
#' services (not used).
#' @param format A \code{character}. Only 'tsv' is used.
#' @param request A \code{character}. Possibel values are 'get_string_ids'
#' (default), network', 'interaction_partners', 'homology', 'homology_best',
#' 'enrichment', 'functional_annotation', 'ppi_enrichment' or 'version'.
#' @param parameters A \code{list}. Different requests require different
#' parameters. The allowed parameters are documented in individual request
#' functions.
#'
#' @return A \code{character} string of the URL.
#'
#' @examples
#' # make a resolve request for PTCH1
#' make_url(parameters = list(identifier = 'PTCH1'))
#'
#' @importFrom httr modify_url
#'
#' @export
make_url <- function(database = 'string-db.org', access = 'api', format = 'tsv',
                     request = 'get_string_ids', parameters = NULL) {
  # check database host name
  #databases <- c('string-db.org', 'string.embl.de', 'stitch.embl.de')
  databases <- 'string-db.org'
  if(!database %in% databases) {
    stop('Provide valid database host.')
  } else {
    hostname <- database
  }

  # check and construct path
  ## access
  #accesses <- c('api', 'services')
  accesses <- 'api'
  stopifnot(access %in% accesses)

  ## format
  #formats <- c('tsv', 'json', 'tsv-no-header', 'psi-mi', 'psi-mi-tab')
  formats <- 'tsv'
  stopifnot(format %in% formats)

  ## request
  requests <- c('get_string_ids', 'network', 'interaction_partners',
                'homology', 'homology_best','enrichment',
                'functional_annotation', 'ppi_enrichment', 'version')
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
                    scheme = 'https',
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
#' # make a resolve request for PTCH1
#' make_url(parameters = list(identifier = 'PTCH1'))
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
#' \dontrun{
#' # make a resolve request for PTCH1
#' make_url(parameters = list(identifier = 'PTCH1'))
#'
#' # send request
#' resp <- send_request(url)
#'
#' # format content
#' format_content(resp, format = 'tsv')
#' }
#'
#' @importFrom httr has_content content
#' @importFrom readr read_tsv
#'
#' @export
format_content <- function(resp, format = 'tsv') {
  # check resp object hase contents
  if(has_content(resp)) {
    # extract content
    cont <- content(resp, as = 'text')
  } else {
    # or stop
    stop("resp has no content.")
  }

  # read the data in the appropriate format
  res <- switch(format,
                'tsv' = read_tsv(cont))

  # return res
  return(res)
}

#' Build a query list
#'
#' @param identifiers A \code{character} string.
#' @param background_string_identifiers A \code{character} string.
#' @param echo_query A \code{logical} in the form '0' (default) or '1'.
#' @param species A \code{numeric}.
#' @param species_b A \code{numeric}.
#' @param limit A \code{numeric}.
#' @param required_score A \code{numeric}. Values are from 0 to 1000.
#' @param add_nodes A \code{numeric}.
#' @param allow_pubmed A \code{logical} in the form '1' (default) or '0'.
#' @param caller_identity A \code{character} string.
#'
#' @return A \code{list}.
#'
#' @examples
#' # build a query list for 'PTCH1' species 9606
#' build_query(identifiers = 'PTCH1',
#'             species = 9606)
#'
#' @export
build_query <- function(identifiers, background_string_identifiers,
                        echo_query, species, species_b, limit, required_score,
                        add_nodes, allow_pubmed, caller_identity) {
  # construct parameters list (param)
  ## make and empty list
  param <- list()

  ## check and add identifiers
  if(is.null(identifiers)) {
    # stop if NULL
    stop("identifiers can't be null.")
  }

  if(length(identifiers) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifiers, collapse = '%0d'))
  } else {
    # or, use identifiers
    param$identifier <- identifiers
  }

  ## optional args
  if(!missing(background_string_identifiers)) {
    ## check and add background_string_identifiers
    if(length(background_string_identifiers) > 1) {
      # if length more than one use identifiers
      param$background_string_identifiers <- I(paste(background_string_identifiers, collapse = '%0d'))
    } else {
      # or, use identifiers
      param$background_string_identifiers <- background_string_identifiers
    }
  }
  if(!missing(echo_query)) {
    ## check and add echo_query
    if(!echo_query %in% c(0, 1)) {
      # stop if not 0 or 1
      stop('echo_query should be a 0 or 1.')
    }

    param$echo_query <- echo_query
  }

  if(!missing(species)) {
    ## check and add species
    if(!is.numeric(species)) {
      # stop if not numeric
      stop('species should be a numeric.')
    }

    param$species <- species
  }

  if(!missing(species_b)) {
    ## check and add species_b
    if(!is.numeric(species_b)) {
      # stop if not numeric
      stop('species_b should be a numeric.')
    }

    param$species_b <- species_b
  }

  if(!missing(limit)) {
    ## check and add limit
    if(!is.numeric(limit)) {
      # stop if not numeric
      stop('limit should be a numeric.')
    }

    param$limit <- limit
  }

  ## check and add required_score
  if(!missing(required_score)) {
    param$required_score <- required_score
  }

  if(!missing(add_nodes)) {
    ## check and add add_nodes
    if(!is.numeric(add_nodes)) {
      # stop if not 0 or 1
      stop('add_nodes should be a numeric.')
    }

    param$add_nodes <- add_nodes
  }

  if(!missing(allow_pubmed)) {
    ## check and add allow_pubmed
    if(!allow_pubmed %in% c(0, 1)) {
      # stop if not numeric
      stop('add_nodes should be a 0 or 1.')
    }

    param$allow_pubmed <- allow_pubmed
  }

  ## check and add caller_identity
  ## not implemented!

  # return param
  return(param)
}
