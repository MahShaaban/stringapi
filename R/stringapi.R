query_map_string <- readr::read_csv(system.file('extdata',
                                                'query_map_string.csv',
                                                package = 'stringapi'))
usethis::use_data(query_map_string, overwrite = TRUE)

#' Query Map for STRING API (v11)
#'
#' The object documents the different types of requests and their query
#' paramaters that are allowed by the API.
"query_map_string"
