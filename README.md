[![Travis build status](https://travis-ci.org/MahShaaban/stringapi.svg?branch=master)](https://travis-ci.org/MahShaaban/stringapi)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/MahShaaban/stringapi?branch=master&svg=true)](https://ci.appveyor.com/project/MahShaaban/stringapi)
[![Codecov test coverage](https://codecov.io/gh/MahShaaban/stringapi/branch/master/graph/badge.svg)](https://codecov.io/gh/MahShaaban/stringapi?branch=master)

# stringapi

An R client for STRING API

## Overview

Provide a set of functions to interact with the [STRING](https://string-db.org/cgi/input.pl) API in R. The functions are organized a round the API database and request types. The query parameters are checked and the output is returned in a tibble.

## Installing `stringapi`

The package can be installed using `devtools`

```r
devtools::install_github('MahShaaban/stringapi')
```

## Getting started

A simple example to show how the package works is to contrast with an example query using `curl`

```bash
curl http://string-db.org/api/tsv/resolve?identifier=ADD&species=9606
```

This would look like the following using `stringapi`

```r
get_resolve(identifier = 'ADD',
            species = 9606)
```

## Acknowledgement

* This implementation is based on the STRING/STITCH API documentation, [here](http://stitch.embl.de/cgi/help.pl?UserId=qZfIPe69o9b4&sessionId=9MtGdB15CK8v).
* **Best practices for API packages** guide was a very useful resource,[here](https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html)
