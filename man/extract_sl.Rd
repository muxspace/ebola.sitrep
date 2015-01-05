\name{extract_sl}
\alias{extract_sl}
\title{Download situation reports from the Sierra Leone Ministry of Health}
\usage{
  extract_sl(url = .SL_SITE, base = .SL_DIR)
}
\arguments{
  \item{url}{The URL of the list of situation reports}

  \item{base}{The base directory to save data files}
}
\value{
  The list of local file names
}
\description{
  Extract all report links from the Ministry of Health web
  site and download each situation report locally.
}
\details{
  This function also converts each PDF into a text file and
  does preprocessing to simplify parsing.
}
\examples{
\dontrun{
fs <- extract_sl()
intersect(fs, files_sl())
}
}
\author{
  Brian Lee Yung Rowe
}

