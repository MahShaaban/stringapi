
#' @export
get_resolve <- function(identifier = NULL, species = NULL, format = 'only-ids',
                        db = 'string') {
  # check if required args are nul
  if(is.null(identifier) || is.null(species)) {
    stop("identifier and species can't be null.")
  }

  # check proper formate
  formats <- c('only-ids', 'full')
  stopifnot(format %in% formats)

  # construct parameters list
  param <- list()
  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
  } else {
    # or, use identifier
    param$identifier <- identifier
  }

  # add species and formate
  param$species <- species
  param$format <- format

  # make database (hostname)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')
  # make url
  url <- make_url(database = database, parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}



#' @export
get_abstracts <- function(identifier = NULL, limit = 5, format = 'pmids',
                          db = 'string') {
  # check if required args are nul
  if(is.null(identifier)) {
    stop("identifier can't be null.")
  }

  # check proper formate
  formats <- c('pmids', 'colon')
  stopifnot(format %in% formats)

  # construct parameters list
  # construct parameters list
  param <- list()
  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
    request <- 'abstractsList'
  } else {
    # or, use identifier
    param$identifier <- identifier
    request <- 'abstracts'
  }

  # add limit and format
  param$limit <- limit
  param$format <- format

  # make database (hostname)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')

  # make url
  url <- make_url(database = database, request = request, parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}


#' @export
get_actions <- function(identifier = NULL, limit = 5, required_score,
                        additional_network_nodes, db = 'string') {
  # check if required args are nul
  if(is.null(identifier)) {
    stop("identifier can't be null.")
  }

  # construct parameters list
  param <- list()
  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
    request <- 'actionsList'
  } else {
    # or, use identifier
    param$identifier <- identifier
    request <- 'actions'
  }

  # add limit and format
  param$limit <- limit
  if(!missing(required_score)) {
    param$required_score <- required_score
  }

  if(!missing(additional_network_nodes)) {
    param$additional_network_nodes <- additional_network_nodes
  }

  # make database (hostname)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')

  # make url
  url <- make_url(database = database, request = request, parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}


#' @export
get_interactors <- function(identifier = NULL, limit = 5, required_score,
                            additional_network_nodes, db = 'string') {
  # check if required args are nul
  if(is.null(identifier)) {
    stop("identifier can't be null.")
  }

  # construct parameters list
  param <- list()
  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
    request <- 'interactorsList'
  } else {
    # or, use identifier
    param$identifier <- identifier
    request <- 'interactors'
  }

  # add limit and format
  param$limit <- limit
  if(!missing(required_score)) {
    param$required_score <- required_score
  }

  if(!missing(additional_network_nodes)) {
    param$additional_network_nodes <- additional_network_nodes
  }

  # make database (hostname)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')

  # make url
  url <- make_url(database = database, request = request, parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' @export
get_interactions <- function(identifier = NULL, limit = 5, required_score,
                             additional_network_nodes, db = 'string') {
  # check if required args are nul
  if(is.null(identifier)) {
    stop("identifier can't be null.")
  }

  # construct parameters list
  param <- list()
  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
    request <- 'interactionsList'
  } else {
    # or, use identifier
    param$identifier <- identifier
    request <- 'interactions'
  }

  # add limit and format
  param$limit <- limit
  if(!missing(required_score)) {
    param$required_score <- required_score
  }

  if(!missing(additional_network_nodes)) {
    param$additional_network_nodes <- additional_network_nodes
  }
  # make database (hostname)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')

  # make url
  url <- make_url(database = database, request = request, parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}


#' @export
get_network <- function(identifier = NULL, limit = 5, required_score,
                        additional_network_nodes, db = 'string',
                        netwok_flavor = 'evidence') {
  # check if required args are nul
  if(is.null(identifier)) {
    stop("identifier can't be null.")
  }

  # check proper network_flavor arg
  netwok_flavors <- c('evidence', 'confidence', 'actions')
  stopifnot(netwok_flavor %in% netwok_flavors)

  # construct parameters list
  param <- list()
  if(length(identifier) > 1) {
    # if length more than one use identifiers
    param$identifiers <- I(paste(identifier, collapse = '%0D'))
    request <- 'networkList'
  } else {
    # or, use identifier
    param$identifier <- identifier
    request <- 'network'
  }

  # add limit and format
  param$limit <- limit
  if(!missing(required_score)) {
    param$required_score <- required_score
  }

  if(!missing(additional_network_nodes)) {
    param$additional_network_nodes <- additional_network_nodes
  }

  # make database (hostname)
  if(!db %in% c('string', 'stitch')) {
    stop('db can only be string or stitch')
  }

  database <- switch(db,
                     'string' = 'string-db.org',
                     'stitch' = 'stitch.embl.de')

  # make url
  url <- make_url(database = database, request = request, parameters = param)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}
