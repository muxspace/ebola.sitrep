# :vim set ft=R
# Parse errors
# . 2014-11-29 - dead.total
# . 2014-11-29 - dead.suspect
# . 2014-11-29 - dead.probable
# . 2014-11-29 - dead.community
# . 2014-11-29 - dead.patients
# . 2014-12-05 - dead.patients
# . 2014-12-02 : present - cum.hcw.cases
# . 2014-12-02 : present - cum.hcw.deaths
# . 2014-11-29 - new.contacts
# Data errors (from source PDFs)
# . 2014-12-06 - alive.probable
# . 2014-12-12 - alive.total
# . 2014-12-07 - under.followup
# . 2014-12-08 - under.followup
# . 2014-11-29 - seen.on.day
# . 2014-12-07 - seen.on.day
# . 2014-12-08 - seen.on.day
# . 2014-11-29 - completed.21d.followup
# . 2014-11-29 - lost.followup
# . 2014-12-05 - lost.followup
# . 2014-12-12 : 2014-12-13 - cum.total
# . 2014-12-12 : 2014-12-13 - cum.suspect

flog.threshold(DEBUG, name='ebola.sitrep')

#' Parse a complete dataset of situation reports for a country
#'
#' Use this function to parse all downloaded and converted PDFs for a given
#' country. The result will be a single data.frame where each row
#' represents the parsed data for a given county and date.
#'
#' @name parse_nation
#' @param fs A vector of situation report file names
#' @return A data.frame representing all situation reports for a country
#' @examples
#' \dontrun{
#' fs <- extract_sl()
#' data <- parse_nation(fs)
#' forecast(data)
#' }
parse_nation <- function(fs) {
  o <- lapply(fs, function(f) parse_file(f))
  cols <- fold(o, function(i, acc) union(acc,colnames(i)), c())
  o <- lapply(o, function(x) { x[,setdiff(cols,colnames(x))] <- NA; x })
  o <- do.call(rbind, o)
  o[order(o$date,o$county),]
}

#' Parse a single file based on the file name
#'
#' @name parse_file
#' @param file.name The situation report to parse
#' @return A data.frame representing a single situation report
parse_file <- function(file.name) {
  types <- c(lr='^SITRep',sl='^Ebola-Situation-Report')
  type <- names(types)[sapply(types, function(x) grepl(x,file.name))]
  if (! type %in% c('lr','sl')) stop("Incorrect type")

  flog.info("[%s] Process %s", type, file.name)
  cmd <- sprintf('parse_%s',type)
  do.call(cmd, list(file.name))
}

#' Write a parsed set of situation reports to disk
#'
#' Writes a unified dataset of country situation reports as a CSV.
#'
#' @param df A unified dataset for a given nation
#' @param path Path to save data.frame
#' @examples
#' \dontrun{
#' fs <- extract_sl()
#' data <- parse_nation(fs)
#' export_nation(data, './data/sitrep_sl.csv')
#' }
export_nation <- function(df, path) {
  write.csv(df, path, row.names=FALSE)
}

#' Validate a dataset
#'
#' Check the consistency of a dataset by comparing the sum of county data
#' with national data. Hence, the sum of each series over all counties
#' must equal the national value. There are instances where the data in
#' the report is wrong.
#'
#' This function shows how filters are used practically. The subsetting
#' operations are shorthand predicates.
#'
#' @name validate
#' @param data The dataset
#' @param measure The measure to validate
#' @param nation The county name that identifies national data
#' @return An nx2 matrix containing the sum of counties and national value
#' @author Brian Lee Yung Rowe
#' @examples
#' \dontrun{
#' fs <- files_lr()
#' data <- parse_lr(fs[1])
#' validate(data,'alive.total')
#' }
validate <- function(data, measure, nation='National') {
  dates <- unique(data$date)
  day_fn <- function(d) {
    day <- data[data$date==d,]
    counties <- sum(day[day$county!=nation,measure])
    national <- day[day$county==nation,measure]
    c(counties=counties, national=national)
  }
  #debug(day_fn)
  o <- t(sapply(dates, day_fn))
  rownames(o) <- format(dates)
  o
}

#' Forecast the value of a measure
#'
#' Given a country dataset, forecast a given measure for the given county.
#'
#' This is designed for Sierra Leone data, which is a bit more consistent
#'
#' @name forecast
#' @param data A complete country dataset
#' @param county The county to use
#' @param measure The measure to forecast
#' @param window How many days to forecast
#' @return A vector of forecasted values for the given measure and county
#' @author Brian Lee Yung Rowe
#' @examples
#' \dontrun{
#' fs <- files_lr()
#' data <- parse_nation(fs)
#' forecast(data)
#' }
forecast <- function(data, county='National', measure='cum.dead.confirmed', window=30) {
  small <- data[data$county==county,]
  date <- small$date
  response <- ifelse(small[,measure] == 0, 0, log(small[,measure]))
  o <- lm(response ~ date)
  fit.dates <- with(small, date[as.numeric(names(o$fitted.values))])
  end <- max(small$date, na.rm=TRUE)
  future.dates <- end + (1:window)
  future.values <- predict(o, data.frame(date=future.dates))
  plot(c(fit.dates,future.dates), exp(c(o$fitted.values,future.values)),
    type='l', lty=3, col='blue', main=county, xlab='date', ylab=measure)
  lines(small$date, small[,measure])
  data.frame(county=county, measure=measure, date=future.dates, value=future.values)
}


################################## PRIVATE ###################################
#' Replace the file extension in a file name
#'
#' Private
#' @name replace_ext
#' @param file A file name
#' @param ext The extension to use as replacement
#' @return A new file name
replace_ext <- function(file, ext) {
  parts <- strsplit(file, ".", fixed=TRUE)
  sapply(parts, function(x) paste(c(x[1:(length(x)-1)], ext), collapse='.'))
}


#' Extract URLs from an XML document
#'
#' Only URLs that match a specific pattern will be returned.
#'
#' Private
#' @name find_urls
#' @param x XML
#' @param pattern A pattern for URLs to find
#' @return A charater vector of matching URLs
find_urls <- function(x, pattern) {
  link <- xmlAttrs(x)['href']
  link <- grep(pattern,link, value=TRUE)
  gsub(' ','%20',link, fixed=TRUE)
}

#' Download and convert a PDF
#'
#' Private
#' @name extract_pdf
#' @param url The location of the PDF
#' @param file The destination file name
#' @param base Directory to save files
extract_pdf <- function(url, file, base) {
  flog.info("GET %s",url)
  r <- GET(url)
  pdf <- content(r,'raw')
  pdf.dest <- paste(base,file,sep='/')
  writeBin(pdf, pdf.dest)

  txt.dest <- replace_ext(pdf.dest,'txt')
  sed <- paste("sed -e 's/<page number=\"\\([0-9]*\\)\"[^>]*>/PAGE-\\1/'",
    "-e 's/<[^>]*>//g'",
    "-e 's/[^[:space:]0-9a-zA-Z-]//g'")
  cmd <- "pdftohtml" 
  args <- sprintf("-i -xml -stdout '%s' | %s", pdf.dest,sed)
  flog.debug("Call `%s %s`",cmd,args)
  system2(cmd, args, stdout=txt.dest)
}

#' Extract a chunk of data from a converted file
#'
#' Private
#' @name get_chunk
#' @param lines Lines from the raw table
#' @param start.marker Regular expression indicating start of parse region
#' @param stop.marker Regular expression indicating end of parse region
#' @param regex A list of patterns and replacements for lines between 
#' start.marker and stop.marker
get_chunk <- function(lines, start.marker, stop.marker, regex) {
  start.idx <- grep(start.marker,lines)
  if (length(start.idx) < 1) 
    flog.warn("[get_chunk] No match for start marker %s",start.marker)
  stop.idx <- grep(stop.marker,lines[(1+start.idx):length(lines)])[1]
  if (is.na(stop.idx)) stop.idx <- length(lines) - start.idx
  
  chunk <- lines[start.idx:(start.idx+stop.idx)]
  chunk <- fold(regex, function(r,ch) sub(r[[1]],r[[2]],ch), chunk)
  grep('^$',chunk, invert=TRUE, value=TRUE)
}

#' Get the index associated with a marker in a raw parsed file
#'
#' Private
#' @name get_index
#' @param raw.table Lines of a raw table
#' @param markers Regexes representing the start of a section (row) in a table
#' @return A vector of indices
get_index <- function(raw.table, markers) {
  section.idx <- lapply(markers, function(x) grep(sprintf('^%s$',x), raw.table))
  # TODO: Add check for NULL
  do.call(c, section.idx)
}

#' Extract a series from a raw table
#' 
#' This function is used whenver data is in contiguous lines following a
#' matching marker. Each line is assumed to be numeric and represents a
#' single datum.
#'
#' Private
#' @name get_series
#' @param raw.table Lines of a raw table
#' @param index Index of a matching marker
#' @param size Number of records to get
get_series <- function(raw.table, index, size) {
  chunk <- raw.table[(index+1):(index+size)]
  as.numeric(gsub('[^0-9]','', chunk))
}

#' Get data as columns from a raw table
#'
#' Private
#' @name get_cols
#' @param lines Lines from the raw table
#' @param start.marker Regular expression indicating start of parse region
#' @param stop.marker Regular expression indicating end of parse region
#' @param markers Regexes representing the start of a section (row) in a table
#' @param labels The names for the columns in the table
#' @param regex A list of patterns and replacements for lines between 
#' start.marker and stop.marker
get_cols <- function(lines, start.marker, stop.marker, markers, labels, regex) {
  size <- length(labels)
  raw.table <- get_chunk(lines, start.marker, stop.marker, regex)
  indices <- get_index(raw.table, markers)
  columns <- lapply(indices, function(i) get_series(raw.table,i,size))
  names(columns) <- raw.table[indices]
  as.data.frame(columns)
}

#' Get data as rows from a raw table
#'
#' Private
#' @name get_rows
#' @param lines Lines from the raw table
#' @param start.marker Regular expression indicating start of parse region
#' @param stop.marker Regular expression indicating end of parse region
#' @param markers Regexes representing the start of a section (row) in a table
#' @param labels The names for the columns in the table
#' @param regex A list of patterns and replacements for lines between 
#' start.marker and stop.marker
get_rows <- function(lines, start.marker, stop.marker, markers, labels, regex) {
  size <- length(labels)
  raw.table <- get_chunk(lines, start.marker, stop.marker, regex)
  indices <- get_index(raw.table, markers)
  rows <- lapply(indices, function(i) get_series(raw.table,i,size))
  df <- as.data.frame(do.call(rbind, rows))
  colnames(df) <- labels
  df$county <- sub('[[:space:]]*$', '', raw.table[indices])
  df
}

