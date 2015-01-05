\name{find_urls}
\alias{find_urls}
\title{Extract URLs from an XML document}
\usage{
  find_urls(x, pattern, clean_fn = function(x) x)
}
\arguments{
  \item{x}{XML}

  \item{pattern}{A pattern for URLs to find}

  \item{clean_fn}{A function that performs additional
  cleaning}
}
\value{
  A charater vector of matching URLs
}
\description{
  Only URLs that match a specific pattern will be returned.
}
\details{
  Private
}

