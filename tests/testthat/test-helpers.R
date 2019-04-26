context("test-helpers")

test_that("test make_url", {
  url1 <- 'https://string-db.org/api/tsv/network?identifiers=PTCH1'
  url2 <- make_url(request = 'network',
                   parameters = list(identifiers = 'PTCH1'))
  expect_identical(url1, url2)

  url1 <- 'https://string-db.org/api/tsv/network?identifiers=PTCH1%0dSHH%0dGLI1%0dSMO%0dGLI3'
  url2 <- make_url(request = 'network',
                   parameters = list(identifiers = I('PTCH1%0dSHH%0dGLI1%0dSMO%0dGLI3')))
  expect_identical(url1, url2)

  expect_error(make_url())
  expect_error(make_url('string-db.net'))
  expect_error(make_url(access = 'API'))
  expect_error(make_url(format = 'csv'))
  expect_error(make_url(parameters = c(identifier = 'PTCH1')))
})

test_that("test send_request", {
  good_url <- 'https://string-db.org/api/tsv/network?identifiers=PTCH1'
  expect_true(httr::has_content(send_request(good_url)))

  bad_url <- 'https://string-db.org/api/tsv/network?identifiers=notPTCH1'
  expect_error(send_request(bad_url))
})

test_that("test build_query", {
  param1 <- build_query(identifiers = 'PTCH1')

  expect_true(is.list(param1))

  param <- build_query(identifiers = 'PTCH1',
                       species = 9606)

  expect_equal(names(param), c('identifier', 'species'))

  expect_error(build_query(species = 9606))

  expect_error(build_query(identifiers = 'PTCH1',
                           limit = '5'))

  expect_error(build_query(identifiers = NULL))
  expect_error(build_query(identifiers = 'PTCH1', echo_query = '2'))
  expect_error(build_query(identifiers = 'PTCH1', allow_pubmed = '2'))
  expect_error(build_query(identifiers = 'PTCH1', species = '9606'))
  expect_error(build_query(identifiers = 'PTCH1', species_b = '9606'))
  expect_error(build_query(identifiers = 'PTCH1', add_nodes = '5'))

})

test_that("test format_content", {
  url <- 'https://string-db.org/api/tsv/get_string_ids?identifiers=p53'
  resp <- httr::GET(url)
  resp$content <- NULL

  expect_error(format_content(resp))
})
