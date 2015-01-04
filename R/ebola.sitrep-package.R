# :vim set ft=R
#' Automatic extraction and parsing of ebola situation reports
#'
#' This package automates the extraction and parsing of
#' situation reports from the health ministries of Liberia and Sierra Leone.
#' From a pedagogical perspective, it serves as a case study in the author's
#' forthcoming book, Modeling Data With Functional Programming In R.
#'
#' \tabular{ll}{
#' Package: \tab ebola.sitrep\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2015-01-04\cr
#' License: \tab What license is it under?\cr
#' }
#'
#' Source ebola data comes directly from the health ministries in African
#' countries. While these sources have the most current data, 
#' the data is not available in a machine-readable format.
#' Most data are published as PDFs, although in some cases the 
#' included tables are merely images.
#' The quality of the data is also poor, with frequent inconsistencies.
#' Other repositories tend to manually transcribe the data into a
#' machine readable format. As a case study for the application of
#' functional programming to data analysis, this package uses
#' functional programming concepts to automatically download and parse
#' the data. The goal is to both provide automated parsing of the documents
#' and also highlight the utility of functional programming to simplify
#' this process.
#'
#' Current support is for Liberia and Sierra Leone. Complete histories
#' can be parsed, but the parsing is inconsistent in quality. The main
#' difficulty is that PDFs do not preserve tables very well, so 
#' parsing relies on many heuristics that are not always satisfied.
#' Prior to using a particular series, it's important to run the 
#' \code{validate} function, which does a minimal consistency check against
#' the national summary values and the constituent counties.
#'
#' The data itself is provided as two consolidated datasets: one for
#' Liberia and one for Sierra Leone. Both county-level and national
#' data is available on a per day basis.
#'
#' @name ebola.sitrep-package
#' @aliases ebola.sitrep-package ebola.sitrep
#' @docType package
#' @exportPattern "^[^\\.]"
#' @import futile.logger lambda.tools RCurl httr scrapeR XML
#' @author Brian Lee Yung Rowe <r@@zatonovo.com>
#' @keywords package attribute logic
#' @examples
#' # For Liberia
#' \dontrun{
#' docs.lr <- extract_lr()
#' data.lr <- parse_all(docs.lr, type='lr')
#' validate(data.lr,'alive.total')
#' with(data.lr[data.lr$county=='NATIONAL',], plot(date,alive.total,type='l'))
#' forecast(data.lr,'NATIONAL', 'cum.dead')
#' }
#'
#' # For Sierra Leone
#' \dontrun{
#' docs.sl <- extract_sl()
#' data.sl <- parse_all(docs.sl, type='sl')
#' validate(data.sl,'alive.total')
#' forecast(data.sl, measure='cum.dead.confirmed')
#' }
NULL
