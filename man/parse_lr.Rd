\name{parse_lr}
\alias{parse_lr}
\title{Parse a situation report into a data.frame}
\usage{
  parse_lr(file.name, base = .LR_DIR)
}
\arguments{
  \item{file.name}{The name of the situation report}

  \item{base}{The base directory to look for the situation
  report}
}
\value{
  A data.frame representing thea situation report
}
\description{
  Parses a single situation report based on the format of
  the report.
}
\details{
  The bulk of the normalization work is in this function.
  As the contents of the situation reports change over
  time, not all data are available.
}
\examples{
\dontrun{
fs <- files_lr()
report <- parse_lr(fs[1])
}
}
\author{
  Brian Lee Yung Rowe
}

