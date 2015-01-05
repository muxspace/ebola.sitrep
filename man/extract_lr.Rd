\name{extract_lr}
\alias{extract_lr}
\title{Download situation reports from the Liberia Ministry of Health}
\usage{
  extract_lr(url = .LR_SITE, base = .LR_DIR)
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
fs <- extract_lr()
intersect(fs, files_lr())
}
}
\author{
  Brian Lee Yung Rowe
}

