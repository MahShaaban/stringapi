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
  # decide request type
  if(length(identifier) > 1) {
    request <- 'resolveList'
  } else {
    request <- 'resolve'
  }

  # construct query
  param <- build_query(request,
                       identifier = identifier,
                       species = species,
                       format = format)

  # construct hostname
  if(missing(db)) {
    db <- 'string'
  }
  database <- build_hostname(db)

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
  # decide request type
  if(length(identifier) > 1) {
    request <- 'abstractsList'
  } else {
    request <- 'abstracts'
  }

  # construct query
  param <- build_query(request,
                       identifier = identifier,
                       limit = limit,
                       format = format)

  # construct hostname
  if(missing(db)) {
    db <- 'string'
  }
  database <- build_hostname(db)

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
  # decide request type
  if(length(identifier) > 1) {
    request <- 'actionstsList'
  } else {
    request <- 'actions'
  }

  # construct query
  param <- build_query(request,
                       identifier = identifier,
                       limit = limit,
                       required_score = required_score,
                       additional_network_nodes = additional_network_nodes)

  # construct hostname
  if(missing(db)) {
    db <- 'string'
  }
  database <- build_hostname(db)

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
  # decide request type
  if(length(identifier) > 1) {
    request <- 'interactorsList'
  } else {
    request <- 'interactors'
  }

  # construct query
  param <- build_query(request,
                       identifier = identifier,
                       limit = limit,
                       required_score = required_score,
                       additional_network_nodes = additional_network_nodes)

  # construct hostname
  if(missing(db)) {
    db <- 'string'
  }
  database <- build_hostname(db)

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
  # decide request type
  if(length(identifier) > 1) {
    request <- 'interactionsList'
  } else {
    request <- 'interactions'
  }

  # construct query
  param <- build_query(request,
                       identifier = identifier,
                       limit = limit,
                       required_score = required_score,
                       additional_network_nodes = additional_network_nodes)

  # construct hostname
  if(missing(db)) {
    db <- 'string'
  }
  database <- build_hostname(db)

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
