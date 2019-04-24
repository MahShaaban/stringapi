context("test-helpers")

test_that("test make_url", {
  url1 <- 'http://string-db.org/api/tsv/resolve?identifier=ADD&species=9606'
  url2 <- make_url(request = 'resolve',
                   parameters = list(identifier='ADD', species=9606))
  expect_identical(url1, url2)

  url1 <- 'http://string-db.org/api/json/interactions?identifier=ADD&species=9606'
  url2 <- make_url(request = 'interactions',
                   format = 'json',
                   parameters = list(identifier='ADD',species=9606))
  expect_identical(url1, url2)

  expect_error(make_url())
  expect_error(make_url('string-db.net'))
  expect_error(make_url(access = 'API'))
  expect_error(make_url(format = 'csv'))
})

test_that("replicate other examples.", {
  url1 <- 'http://string-db.org/api/tsv-no-header/resolve?identifier=YOL086C&format=only-ids'
  url <- make_url(format = 'tsv-no-header',
                  parameters = list(identifier='YOL086C', format='only-ids'))
  expect_identical(url, url1)

  url2 <- 'http://string-db.org/api/tsv/abstractsList?identifiers=4932.YML115C%0D4932.YJR075W%0D4932.YEL036C'
  url <- make_url(format = 'tsv',
                  request = 'abstractsList',
                  parameters = list(identifiers=I("4932.YML115C%0D4932.YJR075W%0D4932.YEL036C")))
  expect_identical(url, url2)

  url3 <- 'http://string-db.org/api/tsv-no-header/interactorsList?identifiers=4932.YML115C%0D4932.YJR075W%0D4932.YEL036C&required_score=400&limit=20'
  url <- make_url(format = 'tsv-no-header',
                  request = 'interactorsList',
                  parameters = list(identifiers=I("4932.YML115C%0D4932.YJR075W%0D4932.YEL036C"),
                                    required_score=400,
                                    limit=20))
  expect_identical(url, url3)
})

test_that("test format_content", {
  fl <- system.file('extdata', 'resp.rda', package = 'stringapi')
  load(fl)

  expect_true(is.data.frame(format_content(resp)))

  resp2 <- resp
  resp2$content <- NULL
  expect_error(format_content(resp2))
})

test_that("test send_request", {
  fl <- system.file('extdata', 'resp.rda', package = 'stringapi')
  load(fl)

  good_url <- 'http://string-db.org/api/tsv/resolve?identifier=ADD&species=9606'
  expect_true(httr::has_content(send_request(good_url)))

  bad_url <- 'http://string-db.org/api/tsv/resolve?identifier=notanid'
  expect_error(send_request(bad_url))
})

test_that("build_query works", {
  param <- build_query('resolve',
                       identifier = 'ADD',
                       species = 9606,
                       format = 'full')
  expect_true(is.list(param))

  param <- build_query('resolve',
                       identifier = 'ADD',
                       format = 'full')

  expect_equal(names(param), c('identifier', 'format'))

  expect_error(build_query('resolve',
                           species = 9606,
                           format = 'full'))
})
