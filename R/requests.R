#' Get a resolve request
#'
#' @param identifier A \code{character} vector.
#' @param species A \code{numeric}.
#' @param format A \code{character} string. Possible values are 'only-ids'
#' (default) or 'full'.
#' @param db A \code{character} string. Possible values are 'string' (default)
#' or 'stitch'.
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # make a resolve request
#' get_resolve(identifier = 'ADD')
#' }
#'
#' @export
get_resolve <- function(identifier = NULL, species = 9606, format = 'only-ids',
                        db = 'string') {
  # construct parameters list (param)
  ## make and empty list
  param <- list()

  ## check and add identifier
  if(is.null(identifier)) {
    # stop if NULL
    stop("identifier can't be null.")
  }

  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
    request <- 'resolveList'
  } else {
    # or, use identifier
    param$identifier <- identifier
    request <- 'resolve'
  }

  ## check and add species
  if(!is.numeric(species)) {
    # stop if not numeric
    stop('species should be a numeric.')
  }

  param$species <- species

  ## check and add format
  if(!format %in% c('only-ids', 'full')) {
    stop("format can only be on of only-ids or full.")
  }

  param$format <- format

  # construct hostname (database)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')

  # make url
  url <- make_url(database = database,
                  parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Get abstracts request
#'
#' @inheritParams get_resolve
#' @param limit A \code{numeric}.
#' @param format A \code{character} string. Possible values are 'pmid'
#' (default) or 'colon'.
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # make abstracts request
#' get_abstracts(identifier = c('4932.YML115C', '4932.YJR075W', '4932.YEL036C'))
#' }
#'
#' @export
get_abstracts <- function(identifier = NULL, limit = 5, format = 'pmid',
                          db = 'string') {
  # construct parameters list (param)
  ## make and empty list
  param <- list()

  ## check and add identifier
  if(is.null(identifier)) {
    # stop if NULL
    stop("identifier can't be null.")
  }

  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
    request <- 'abstractsList'
  } else {
    # or, use identifier
    param$identifier <- identifier
    request <- 'abstracts'
  }

  ## check and add limit
  if(!is.numeric(limit)) {
    # stop if not numeric
    stop('limit should be a numeric.')
  }

  param$limit <- limit

  ## check and add format
  if(!format %in% c('pmid', 'colon')) {
    stop("format can only be on of pmid or colon.")
  }

  param$format <- format

  # construct hostname (database)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')

  # make url
  url <- make_url(database = database,
                  request = request,
                  parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Get actions request
#'
#' @inheritParams get_resolve
#' @inheritParams get_abstracts
#' @param required_score A \code{numeric}.
#' @param additional_network_nodes A \code{numeric}
#'
#' @return A \code{tibble}.
#'
#' @examples
#' \dontrun{
#' # make actions request
#' get_actions(identifier = 'ADD')
#' }
#'
#' @export
get_actions <- function(identifier = NULL, limit = 5, required_score,
                        additional_network_nodes, db = 'string') {
  # construct parameters list (param)
  ## make and empty list
  param <- list()

  ## check and add identifier
  if(is.null(identifier)) {
    # stop if NULL
    stop("identifier can't be null.")
  }

  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
    request <- 'actionsList'
  } else {
    # or, use identifier
    param$identifier <- identifier
    request <- 'actions'
  }

  ## check and add limit
  if(!is.numeric(limit)) {
    # stop if not numeric
    stop('limit should be a numeric.')
  }

  param$limit <- limit

  ## check and add required_score
  if(!missing(required_score)) {
    param$required_score <- required_score
  }

  ## check and add additional_network_nodes
  if(!missing(additional_network_nodes)) {
    param$additional_network_nodes <- additional_network_nodes
  }

  # construct hostname (database)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')

  # make url
  url <- make_url(database = database,
                  request = request,
                  parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Get interactors request
#'
#' @inheritParams get_resolve
#' @inheritParams get_abstracts
#' @inheritParams get_actions
#'
#' @return A \code{tibble}
#'
#' @examples
#' \dontrun{
#' # make interactors request
#' get_interactors(identifier = 'ADD')
#' }
#'
#' @export
get_interactors <- function(identifier = NULL, limit = 5, required_score,
                            additional_network_nodes, db = 'string') {
  # construct parameters list (param)
  ## make and empty list
  param <- list()

  ## check and add identifier
  if(is.null(identifier)) {
    # stop if NULL
    stop("identifier can't be null.")
  }

  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
    request <- 'interactorsList'
  } else {
    # or, use identifier
    param$identifier <- identifier
    request <- 'interactors'
  }

  ## check and add limit
  if(!is.numeric(limit)) {
    # stop if not numeric
    stop('limit should be a numeric.')
  }

  param$limit <- limit

  ## check and add required_score
  if(!missing(required_score)) {
    param$required_score <- required_score
  }

  ## check and add additional_network_nodes
  if(!missing(additional_network_nodes)) {
    param$additional_network_nodes <- additional_network_nodes
  }

  # construct hostname (database)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')

  # make url
  url <- make_url(database = database,
                  request = request,
                  parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Get interactions request
#'
#' @inheritParams get_resolve
#' @inheritParams get_abstracts
#' @inheritParams get_actions
#'
#' @return A \code{tibble}
#'
#' @examples
#' \dontrun{
#' # make interactions request
#' get_interactions(identifier = 'ADD')
#' }
#'
#' @export
get_interactions <- function(identifier = NULL, limit = 5, required_score,
                             additional_network_nodes, db = 'string') {
  # construct parameters list (param)
  ## make and empty list
  param <- list()

  ## check and add identifier
  if(is.null(identifier)) {
    # stop if NULL
    stop("identifier can't be null.")
  }

  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
    request <- 'interactionsList'
  } else {
    # or, use identifier
    param$identifier <- identifier
    request <- 'interactions'
  }

  ## check and add limit
  if(!is.numeric(limit)) {
    # stop if not numeric
    stop('limit should be a numeric.')
  }

  param$limit <- limit

  ## check and add required_score
  if(!missing(required_score)) {
    param$required_score <- required_score
  }

  ## check and add additional_network_nodes
  if(!missing(additional_network_nodes)) {
    param$additional_network_nodes <- additional_network_nodes
  }

  # construct hostname (database)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')

  # make url
  url <- make_url(database = database,
                  request = request,
                  parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}
