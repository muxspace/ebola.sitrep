\name{get_cols}
\alias{get_cols}
\title{Get data as columns from a raw table}
\usage{
  get_cols(lines, start.marker, stop.marker, markers,
    labels, regex)
}
\arguments{
  \item{lines}{Lines from the raw table}

  \item{start.marker}{Regular expression indicating start
  of parse region}

  \item{stop.marker}{Regular expression indicating end of
  parse region}

  \item{markers}{Regexes representing the start of a
  section (row) in a table}

  \item{labels}{The names for the columns in the table}

  \item{regex}{A list of patterns and replacements for
  lines between start.marker and stop.marker}
}
\description{
  Private
}

