\name{files_lr}
\alias{files_lr}
\title{Get file names of downloaded and converted files for Liberia}
\usage{
  files_lr(base = .LR_DIR)
}
\arguments{
  \item{base}{The base directory to look for data files}
}
\value{
  A list of files
}
\description{
  Once files are extracted from the source, they are saved
  locally. This function reads the local directory and
  returns the names of all files to parse and consolidate
  into a single data.frame.
}
\examples{
\dontrun{
x <- files_lr()
}
}
\author{
  Brian Lee Yung Rowe
}

