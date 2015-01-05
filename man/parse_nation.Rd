\name{parse_nation}
\alias{parse_nation}
\title{Parse a complete dataset of situation reports for a country}
\usage{
  parse_nation(fs)
}
\arguments{
  \item{fs}{A vector of situation report file names}
}
\value{
  A data.frame representing all situation reports for a
  country
}
\description{
  Use this function to parse all downloaded and converted
  PDFs for a given country. The result will be a single
  data.frame where each row represents the parsed data for
  a given county and date.
}
\examples{
\dontrun{
fs <- extract_sl()
data <- parse_nation(fs)
forecast(data)
}
}

