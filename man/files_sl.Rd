\name{files_sl}
\alias{files_sl}
\title{Get file names of downloaded and converted files for Sierra Leone}
\usage{
  files_sl(base = .SL_DIR)
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
x <- files_sl()
}
}
\author{
  Brian Lee Yung Rowe
}

