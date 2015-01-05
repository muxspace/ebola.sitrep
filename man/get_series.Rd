\name{get_series}
\alias{get_series}
\title{Extract a series from a raw table}
\usage{
  get_series(raw.table, index, size)
}
\arguments{
  \item{raw.table}{Lines of a raw table}

  \item{index}{Index of a matching marker}

  \item{size}{Number of records to get}
}
\description{
  This function is used whenver data is in contiguous lines
  following a matching marker. Each line is assumed to be
  numeric and represents a single datum.
}
\details{
  Private
}

