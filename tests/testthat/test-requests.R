context("test-requests")

test_that('get_string_ids works', {
  # make a get_string_ids request
  res <- get_string_ids(identifiers = c('p53', 'dcdk2'),
                        species = 9606)

  expect_true(nrow(res) >= 1)
})

test_that('network works', {
  # make a network request
  res <- network(identifiers = c('TP53', 'EGFR', 'CDK2'),
                 species = 9606)

  expect_true(nrow(res) >= 1)
})

test_that('interaction_partners works', {
  # make a network request
  res <- # make a interaction_partners request
    interaction_partners(identifiers = c('TP53', 'CDK2'),
                         limit = 10)

  expect_true(nrow(res) >= 1)
})

test_that('homology works', {
  # make a homology request
  res <- homology(identifiers = c('TP53', 'CDK2'))

  expect_true(nrow(res) >= 1)
})

test_that('homology_best works', {
  # make a homology_best request
  res <- homology_best(identifiers = 'CDK1',
                       species_b = 10090)

  expect_true(nrow(res) >= 1)
})

test_that('enrichment works', {
  # make an enrichment request
  res <- enrichment(identifiers = c('trpA','trpB','trpC','trpE','trpGD'))

  expect_true(is.data.frame(res))
})

test_that('functional_annotation works', {
  # make a functional_annotation request
  res <- functional_annotation(identifiers = 'cdk1')

  expect_true(nrow(res) >= 1)
})

test_that('ppi_enrichment works', {
  # make a ppi_enrichment request
  res <- ppi_enrichment(identifiers = c('trpA','trpB','trpC','trpE','trpGD'))

  expect_true(nrow(res) >= 1)
})
