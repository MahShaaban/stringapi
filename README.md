[![Travis build status](https://travis-ci.org/abifromr/stringapi.svg?branch=master)](https://travis-ci.org/abifromr/stringapi)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/abifromr/stringapi?branch=master&svg=true)](https://ci.appveyor.com/project/abifromr/stringapi)
[![Codecov test coverage](https://codecov.io/gh/abifromr/stringapi/branch/master/graph/badge.svg)](https://codecov.io/gh/abifromr/stringapi?branch=master)

# stringapi

An R client for STRING API

## Overview

Provide a set of functions to interact with the [STRING](https://string-db.org/cgi/input.pl) API in R. The functions are organized a round the API database and request/method types. The query parameters are checked and the output is returned in a `tibble`.

## Installing `stringapi`

The package can be installed using `devtools`

```r
devtools::install_github('abifromr/stringapi')
```

## Getting started

A simple example to show how the package works is to contrast with an example query using `curl`

```bash
curl https://string-db.org/api/tsv/get_string_ids?identifiers=p53%0dcdk2&species=9606

```

This would look like the following using `stringapi`

```r
get_string_ids(identifiers = c('p53', 'dcdk2'),
               species = 9606)
```

## Acknowledgement

* This implementation is based on the STRING API v11 documentation, [here](https://string-db.org/cgi/help.pl?sessionId=Hk0j6heBuDI3).
* **Best practices for API packages** guide was a very useful resource,[here](https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html)
