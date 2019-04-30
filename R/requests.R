#' Mapping identifiers
#'
#' Maps common protein names, synonyms and UniProt identifiers into STRING
#' identifiers.
#'
#' @param identifiers A \code{character} string.
#' @param echo_query A \code{logical} in the form '0' (default) or '1'.
#' @param species A \code{numeric}.
#' @param limit A \code{numeric}.
#' @param caller_identity A \code{character} string.
#'
#' @return A \code{tibble}.
#' \describe{
#'   \item{queryItem}{(OPTIONAL) your input protein}
#'   \item{queryIndex}{position of the protein in your input (starting from
#'   position 0)}
#'   \item{stringId}{STRING identifier}
#'   \item{ncbiTaxonId}{NCBI taxon identifier}
#'   \item{taxonName}{species name}
#'   \item{preferredName}{common protein name}
#'   \item{annotation}{protein annotation}
#' }
#'
#' @family API methods
#'
#' @seealso
#' \code{\link{network}}
#' \code{\link{interaction_partners}}
#' \code{\link{homology}}
#' \code{\link{homology_best}}
#' \code{\link{enrichment}}
#' \code{\link{functional_annotation}}
#' \code{\link{ppi_enrichment}}
#'
#' @source https://string-db.org/cgi/help.pl?subpage=api%23mapping-identifiers
#'
#' @examples
#' \dontrun{
#' # make a get_string_ids request
#' get_string_ids(identifiers = c('p53', 'dcdk2'),
#'                species = 9606)
#' }
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
get_string_ids <- function(identifiers = NULL, echo_query = 0, limit = 5,
                        species = 9606, caller_identity) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'get_string_ids',
    parameters = param,
    query_map = query_map_string
  )

  # make url
  url <- modify_url('',
                    scheme = 'https',
                    hostname = 'string-db.org',
                    path = 'api/tsv/get_string_ids',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Getting the STRING network interactions
#'
#' Retrieves the network interactions for your input protein(s) in various
#' text based formats.
#'
#' @param identifiers A \code{character} string.
#' @param species A \code{numeric}.
#' @param required_score A \code{numeric}. Values are from 0 to 1000.
#' @param add_nodes A \code{numeric}.
#' @param caller_identity A \code{character} string.
#'
#' @return A \code{tibble}.
#' \describe{
#'   \item{stringId_A}{STRING identifier (protein A)}
#'   \item{stringId_B}{STRING identifier (protein B)}
#'   \item{preferredName_A}{common protein name (protein A)}
#'   \item{preferredName_B}{common protein name (protein B)}
#'   \item{ncbiTaxonId}{NCBI taxon identifier}
#'   \item{score}{combined score}
#'   \item{nscore}{gene neighborhood score}
#'   \item{fscore}{gene fusion score}
#'   \item{pscore}{phylogenetic profile score}
#'   \item{ascore}{coexpression score}
#'   \item{escore}{experimental score}
#'   \item{dscore}{database score}
#'   \item{tscore}{textmining score}
#' }
#'
#' @family API methods
#'
#' @seealso
#' \code{\link{get_string_ids}}
#' \code{\link{interaction_partners}}
#' \code{\link{homology}}
#' \code{\link{homology_best}}
#' \code{\link{enrichment}}
#' \code{\link{functional_annotation}}
#' \code{\link{ppi_enrichment}}
#'
#' @source https://string-db.org/cgi/help.pl?subpage=api%23getting-the-string-network-interactions
#'
#' @examples
#' \dontrun{
#' # make a network request
#' network(identifiers = c('TP53', 'EGFR', 'CDK2'),
#'            required_score = 400)
#' }
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
network <- function(identifiers = NULL, species = 9606, required_score,
                    add_nodes, caller_identity) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'network',
    parameters = param,
    query_map = query_map_string
  )

  # make url
  url <- modify_url('',
                    scheme = 'https',
                    hostname = 'string-db.org',
                    path = 'api/tsv/network',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Getting all the STRING interaction partners of the protein set
#'
#' Gets all the STRING interaction partners of your proteins.
#'
#' @param identifiers A \code{character} string.
#' @param species A \code{numeric}.
#' @param limit A \code{numeric}.
#' @param required_score A \code{numeric}. Values are from 0 to 1000.
#' @param caller_identity A \code{character} string.
#'
#' @return A \code{tibble}.
#' \describe{
#'   \item{stringId_A}{STRING identifier (protein A)}
#'   \item{stringId_B}{STRING identifier (protein B)}
#'   \item{preferredName_A}{common protein name (protein A)}
#'   \item{preferredName_B}{common protein name (protein B)}
#'   \item{ncbiTaxonId}{NCBI taxon identifier}
#'   \item{score}{combined score}
#'   \item{nscore}{gene neighborhood score}
#'   \item{fscore}{gene fusion score}
#'   \item{pscore}{phylogenetic profile score}
#'   \item{ascore}{coexpression score}
#'   \item{escore}{experimental score}
#'   \item{dscore}{database score}
#'   \item{tscore}{textmining score}
#' }
#'
#' @family API methods
#'
#' @seealso
#' \code{\link{get_string_ids}}
#' \code{\link{network}}
#' \code{\link{homology}}
#' \code{\link{homology_best}}
#' \code{\link{enrichment}}
#' \code{\link{functional_annotation}}
#' \code{\link{ppi_enrichment}}
#'
#' @source https://string-db.org/cgi/help.pl?subpage=api%23getting-all-the-string-interaction-partners-of-the-protein-set
#'
#' @examples
#' \dontrun{
#' # make a interaction_partners request
#' interaction_partners(identifiers = c('TP53', 'CDK2'),
#'                      limit = 10)
#' }
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
interaction_partners <- function(identifiers = NULL, limit = 5, species = 9606,
                                 required_score, caller_identity) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'interaction_partners',
    parameters = param,
    query_map = query_map_string
  )

  # make url
  url <- modify_url('',
                    scheme = 'https',
                    hostname = 'string-db.org',
                    path = 'api/tsv/interaction_partners',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Retrieving similarity scores of the protein set
#'
#' Retrieve the protein similarity scores between the input proteins.
#'
#' @param identifiers A \code{character} string.
#' @param species A \code{numeric}.
#' @param caller_identity A \code{character} string.
#'
#' @return A \code{tibble}.
#' \describe{
#'   \item{stringId_A}{STRING identifier (protein A)}
#'   \item{ncbiTaxonId_A}{NCBI taxon identifier (protein A)}
#'   \item{stringId_B}{STRING identifier (protein B)}
#'   \item{ncbiTaxonId_B}{NCBI taxon identifier (protein B)}
#'   \item{bitscore}{Smith-Waterman alignment bit score}
#' }
#'
#' @family API methods
#'
#' @seealso
#' \code{\link{get_string_ids}}
#' \code{\link{network}}
#' \code{\link{interaction_partners}}
#' \code{\link{homology_best}}
#' \code{\link{enrichment}}
#' \code{\link{functional_annotation}}
#' \code{\link{ppi_enrichment}}
#'
#' @source https://string-db.org/cgi/help.pl?subpage=api%23retrieving-similarity-scores-of-the-protein-set
#'
#' @examples
#' \dontrun{
#' # make a homology request
#' homology(identifiers = c('TP53', 'CDK2'))
#' }
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
homology <- function(identifiers = NULL, species = 9606, caller_identity) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'homology',
    parameters = param,
    query_map = query_map_string
  )

  # make url
  url <- modify_url('',
                    scheme = 'https',
                    hostname = 'string-db.org',
                    path = 'api/tsv/homology',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Retrieving best similarity hits between species
#'
#' Retrieve the best (highest) protein similarity hit between different species
#' in your input.
#'
#' @param identifiers A \code{character} string.
#' @param species A \code{numeric}.
#' @param species_b A \code{numeric}.
#' @param caller_identity A \code{character} string.
#'
#' @return A \code{tibble}.
#' \describe{
#'   \item{stringId_A}{STRING identifier (protein A)}
#'   \item{ncbiTaxonId_A}{NCBI taxon identifier (protein A)}
#'   \item{stringId_B}{STRING identifier (protein B)}
#'   \item{ncbiTaxonId_B}{NCBI taxon identifier (protein B)}
#'   \item{bitscore}{Smith-Waterman alignment bit score}
#' }
#'
#' @family API methods
#'
#' @seealso
#' \code{\link{get_string_ids}}
#' \code{\link{network}}
#' \code{\link{interaction_partners}}
#' \code{\link{homology}}
#' \code{\link{enrichment}}
#' \code{\link{functional_annotation}}
#' \code{\link{ppi_enrichment}}
#'
#' @source https://string-db.org/cgi/help.pl?subpage=api%23retrieving-best-similarity-hits-between-species
#'
#' @examples
#' \dontrun{
#' # make a homology_best request
#' homology_best(identifiers = 'CDK1',
#'               species_b = 10090)
#' }
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
homology_best <- function(identifiers = NULL, species = 9606, species_b,
                          caller_identity) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'homology_best',
    parameters = param,
    query_map = query_map_string
  )

  # make url
  url <- modify_url('',
                    scheme = 'https',
                    hostname = 'string-db.org',
                    path = 'api/tsv/homology_best',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Getting functional enrichment
#'
#' Performs the enrichment analysis of your set of proteins for the Gene
#' Ontology, KEGG pathways, UniProt Keywords, PubMed publications, Pfam,
#' InterPro and SMART domains.
#'
#' @param identifiers A \code{character} string.
#' @param background_string_identifiers A \code{character} string.
#' @param species A \code{numeric}.
#' @param caller_identity A \code{character} string.
#'
#' @return A \code{tibble}.
#' \describe{
#'  \item{category}{term category (e.g. GO Process, KEGG pathways)}
#'  \item{term}{enriched term (GO term, domain or pathway)}
#'  \item{number_of_genes}{number of genes in your input list with the term
#'  assigned}
#'  \item{number_of_genes_in_background}{total number of genes in the
#'  background proteome with the term assigned}
#'  \item{ncbiTaxonId}{NCBI taxon identifier}
#'  \item{inputGenes}{gene names from your input}
#'  \item{preferredNames}{common protein names (in the same order as your
#'  input Genes)}
#'  \item{p_value}{raw p-value}
#'  \item{fdr}{False Discovery Rate}
#'  \item{description}{description of the enriched term}
#' }
#'
#' @family API methods
#'
#' @seealso
#' \code{\link{get_string_ids}}
#' \code{\link{network}}
#' \code{\link{interaction_partners}}
#' \code{\link{homology}}
#' \code{\link{homology_best}}
#' \code{\link{functional_annotation}}
#' \code{\link{ppi_enrichment}}
#'
#' @source https://string-db.org/cgi/help.pl?subpage=api%23getting-functional-enrichment
#'
#' @examples
#' \dontrun{
#' # make an enrichment request
#' enrichment(identifiers = c('trpA','trpB','trpC','trpE','trpGD'))
#' }
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
enrichment <- function(identifiers = NULL, background_string_identifiers,
                       species = 9606, caller_identity) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'enrichment',
    parameters = param,
    query_map = query_map_string
  )

  # make url
  url <- modify_url('',
                    scheme = 'https',
                    hostname = 'string-db.org',
                    path = 'api/tsv/enrichment',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Retrieving functional annotation
#'
#' Gets the functional annotation (Gene Ontology, UniProt Keywords, PFAM,
#' INTERPRO and SMART domains) of your list of proteins.
#'
#' @param identifiers A \code{character} string.
#' @param species A \code{numeric}.
#' @param allow_pubmed A \code{logical} in the form '1' (default) or '0'.
#' @param caller_identity A \code{character} string.
#'
#' @return A \code{tibble}.
#' \describe{
#'  \item{category}{term category (e.g. GO Process, KEGG pathways)}
#'  \item{term}{enriched term (GO term, domain or pathway)}
#'  \item{number_of_genes}{number of genes in your input list with the term
#'  assigned}
#'  \item{ratio_in_set}{ratio of the proteins in your input list with the
#'  term assigned}
#'  \item{ncbiTaxonId}{NCBI taxon identifier}
#'  \item{inputGenes}{gene names from your input}
#'  \item{preferredNames}{common protein names (in the same order as your
#'  input Genes)}
#'  \item{description}{description of the enriched term}
#' }
#'
#' @family API methods
#'
#' @seealso
#' \code{\link{get_string_ids}}
#' \code{\link{network}}
#' \code{\link{interaction_partners}}
#' \code{\link{homology}}
#' \code{\link{homology_best}}
#' \code{\link{enrichment}}
#' \code{\link{ppi_enrichment}}
#'
#' @source https://string-db.org/cgi/help.pl?subpage=api%23retrieving-functional-annotation
#'
#' @examples
#' \dontrun{
#' # make a functional_annotation request
#' functional_annotation(identifiers = 'cdk1')
#' }
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
functional_annotation <- function(identifiers = NULL, species = 9606,
                                  allow_pubmed = 0, caller_identity) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'functional_annotation',
    parameters = param,
    query_map = query_map_string
  )

  # make url
  url <- modify_url('',
                    scheme = 'https',
                    hostname = 'string-db.org',
                    path = 'api/tsv/functional_annotation',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Getting protein-protein interaction enrichment
#'
#' Tests if your network has more interactions than expected.
#'
#' @param identifiers A \code{character} string.
#' @param species A \code{numeric}.
#' @param required_score A \code{numeric}. Values are from 0 to 1000.
#' @param caller_identity A \code{character} string.
#'
#' @return A \code{tibble}.
#' \describe{
#'  \item{number_of_nodes}{number of proteins in your network}
#'  \item{number_of_edges}{number of edges in your network}
#'  \item{average_node_degree}{mean degree of the node in your network}
#'  \item{local_clustering_coefficient}{average local clustering coefficient}
#'  \item{expected_number_of_edges}{expected number of edges based on the nodes
#'   degrees}
#'  \item{p_value}{significance of your network having more interactions than expected}
#' }
#'
#' @family API methods
#'
#' @seealso
#' \code{\link{get_string_ids}}
#' \code{\link{network}}
#' \code{\link{interaction_partners}}
#' \code{\link{homology}}
#' \code{\link{homology_best}}
#' \code{\link{enrichment}}
#' \code{\link{functional_annotation}}
#'
#' @source https://string-db.org/cgi/help.pl?subpage=api%23getting-protein-protein-interaction-enrichment
#'
#' @examples
#' \dontrun{
#' # make a ppi_enrichment request
#' ppi_enrichment(identifiers = c('trpA','trpB','trpC','trpE','trpGD'))
#' }
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
ppi_enrichment <- function(identifiers = NULL, species = 9606, required_score,
                           caller_identity) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'ppi_enrichment',
    parameters = param,
    query_map = query_map_string
  )

  # make url
  url <- modify_url('',
                    scheme = 'https',
                    hostname = 'string-db.org',
                    path = 'api/tsv/ppi_enrichment',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}
