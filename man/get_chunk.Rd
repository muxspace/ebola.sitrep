\name{get_chunk}
\alias{get_chunk}
\title{Extract a chunk of data from a converted file}
\usage{
  get_chunk(lines, start.marker, stop.marker, regex)
}
\arguments{
  \item{lines}{Lines from the raw table}

  \item{start.marker}{Regular expression indicating start
  of parse region}

  \item{stop.marker}{Regular expression indicating end of
  parse region}

  \item{regex}{A list of patterns and replacements for
  lines between start.marker and stop.marker}
}
\description{
  Private
}

