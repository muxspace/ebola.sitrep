\name{export_nation}
\alias{export_nation}
\title{Write a parsed set of situation reports to disk}
\usage{
  export_nation(df, path)
}
\arguments{
  \item{df}{A unified dataset for a given nation}

  \item{path}{Path to save data.frame}
}
\description{
  Writes a unified dataset of country situation reports as
  a CSV.
}
\examples{
\dontrun{
fs <- extract_sl()
data <- parse_nation(fs)
export_nation(data, './data/sitrep_sl.csv')
}
}

