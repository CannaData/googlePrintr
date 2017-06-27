
[![Travis-CI Build Status](https://travis-ci.org/CannaData/googlePrintr.svg?branch=master)](https://travis-ci.org/CannaData/googlePrintr)[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/CannaData/googlePrintr?branch=master&svg=true)](https://ci.appveyor.com/project/CannaData/googlePrintr) <!-- README.md is generated from README.Rmd. Please edit that file -->

googlePrintr
============

The goal of googlePrintr is to connect to the Google Cloud Print API. It is depends on the awesome [`googleAuthR`](https://github.com/MarkEdmondson1234/googleAuthR) package.

Installation
------------

You can install googlePrintr from github with:

``` r
# install.packages("devtools")
devtools::install_github("CannaData/googlePrintr")
```

Example
-------

Printing:

``` r
library(googleAuthR)
library(googlePrintr)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloudprint"))
options("googleAuthR.ok_content_types"= c(getOption("googleAuthR.ok_content_types"),
                                          "text/plain"))

# login using googleAuthR
gar_auth_service("my.json", scope = c("https://www.googleapis.com/auth/cloudprint"))

printer <- gcp_search("myPrinter")

gcp_submit(printer$id[1], 
           "New Title",
           content = 
             "<h1>Hello World</h1>",
           contentType = "text/html"
           )
```
