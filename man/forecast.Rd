\name{forecast}
\alias{forecast}
\title{Forecast the value of a measure}
\usage{
  forecast(data, county = "National",
    measure = "cum.dead.confirmed", window = 30)
}
\arguments{
  \item{data}{A complete country dataset}

  \item{county}{The county to use}

  \item{measure}{The measure to forecast}

  \item{window}{How many days to forecast}
}
\value{
  A vector of forecasted values for the given measure and
  county
}
\description{
  Given a country dataset, forecast a given measure for the
  given county.
}
\details{
  This is designed for Sierra Leone data, which is a bit
  more consistent
}
\examples{
\dontrun{
fs <- files_lr()
data <- parse_nation(fs)
forecast(data)
}
}
\author{
  Brian Lee Yung Rowe
}

