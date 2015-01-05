\name{validate}
\alias{validate}
\title{Validate a dataset}
\usage{
  validate(data, measure, nation = "National")
}
\arguments{
  \item{data}{The dataset}

  \item{measure}{The measure to validate}

  \item{nation}{The county name that identifies national
  data}
}
\value{
  An nx2 matrix containing the sum of counties and national
  value
}
\description{
  Check the consistency of a dataset by comparing the sum
  of county data with national data. Hence, the sum of each
  series over all counties must equal the national value.
  There are instances where the data in the report is
  wrong.
}
\details{
  This function shows how filters are used practically. The
  subsetting operations are shorthand predicates.
}
\examples{
\dontrun{
fs <- files_lr()
data <- parse_lr(fs[1])
validate(data,'alive.total')
}
}
\author{
  Brian Lee Yung Rowe
}

