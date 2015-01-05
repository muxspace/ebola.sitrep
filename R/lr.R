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

LR_DIR <- './data/lr'
LR_SITE <- 'http://www.mohsw.gov.lr/content_display.php?sub=report2'


#' Get file names of downloaded and converted files for Liberia
#'
#' Once files are extracted from the source, they are saved locally.
#' This function reads the local directory and returns the names of 
#' all files to parse and consolidate into a single data.frame.
#'
#' @name files_lr
#' @param base The base directory to look for data files
#' @return A list of files
#' @author Brian Lee Yung Rowe
#' @examples
#' x <- files_lr()
files_lr <- function(base=LR_DIR) {
  list.files(base, 'SITRep.*\\.txt')
}


#' Download situation reports from the Liberia Ministry of Health
#'
#' Extract all report links from the Ministry of Health web site and
#' download each situation report locally.
#'
#' This function also converts each PDF into a text file and does
#' preprocessing to simplify parsing.
#' @name extract_lr
#' @param url The URL of the list of situation reports
#' @param base The base directory to save data files
#' @return The list of local file names
#' @author Brian Lee Yung Rowe
#' @examples
#' \dontrun{
#' fs <- extract_lr()
#' intersect(fs, files_lr())
#' }
extract_lr <- function(url=LR_SITE, base=LR_DIR) {
  page <- scrape(url)
  xpath <- "//div[@id='content']/li/a"
  links <- do.call(c, 
    xpathSApply(page[[1]], xpath, function(x) do_xml(x,'SITRep')))
  files <- sapply(strsplit(links,'/'), function(x) gsub('%20','_',x[length(x)]))

  url.parts <- parse_url(url)
  urls <- paste(url.parts$scheme,"://",url.parts$hostname,'/',links, sep='')

  apply(cbind(urls,files), 1, function(u) do_etl(u[1], u[2], base))
  o <- replace_ext(files,'txt')
  names(o) <- NULL
  o
}


#' Parse a situation report into a data.frame
#'
#' Parses a single situation report based on the format of the report.
#'
#' The bulk of the normalization work is in this function. As the
#' contents of the situation reports change over time, not all data 
#' are available. 
#'
#' @name parse_lr
#' @param file.name The name of the situation report
#' @param base The base directory to look for the situation report
#' @return A data.frame representing thea situation report
#' @author Brian Lee Yung Rowe
#'
#' @examples
#' fs <- files_lr()
#' report <- parse_lr(fs[1])
parse_lr <- function(file.name, base=LR_DIR) {
  lines <- readLines(sprintf('%s/%s',base,file.name), warn=FALSE)
  flog.info("[lr] Found %s raw lines", length(lines))
  if (length(lines) < 1) return(NULL)

    markers <- c('Bomi','Bong','Gbarpolu','Grand Bassa','Grand Cape Mount',
     'Grand Gedeh','Grand Kru','Lofa','Margibi','Maryland','Montserrado',
     'Nimba','River Gee','River Cess','Sinoe','NATIONAL')

  vhf_fn <- function(lines, start.marker, stop.marker, markers, labels, regex) {
    size <- length(labels)
    raw.table <- do_extract(lines, start.marker, stop.marker, regex)
    raw.table <- do.call(c, strsplit(raw.table, '  ', fixed=TRUE))
    start.idx <- get_index(raw.table, 'deaths') + 1
    indices <- start.idx + size * (0:(length(markers)-1))
    rows <- lapply(indices, function(i) get_series(raw.table,i,size))
    df <- as.data.frame(do.call(rbind, rows))
    colnames(df) <- labels
    df$county <- markers
    df
  }

  hcw_fn <- function(lines, start.marker, stop.marker, markers, labels, regex) {
    size <- length(labels)
    raw.table <- do_extract(lines, start.marker, stop.marker, regex)
    raw.table <- do.call(c, strsplit(raw.table, '  ', fixed=TRUE))
    start.idx <- 3
    indices <- start.idx + size * (0:(length(markers)-1))
    rows <- lapply(indices, function(i) get_series(raw.table,i,size))
    df <- as.data.frame(do.call(rbind, rows))
    colnames(df) <- labels
    df$county <- markers
    df
  }


  get_config <- function(file.name) {
    MAX_VERSION <- 299

    start.case.a <- 'Ebola Case and Death Summary by County'
    label.case.a <- c('alive.total','alive.suspect','alive.probable',
      'dead.total',
      'cum.total','cum.suspect','cum.probable','cum.confirm','cum.death')
    label.case.b <- c('_alive.total','_alive.suspect','_alive.probable',
      '_dead.total',
      'cum.total','cum.suspect','cum.probable','cum.confirm','cum.death')

    start.hcw <- 'Healthcare Worker HCW Cases'
    label.hcw <- c('new.hcw.cases','new.hcw.deaths','cum.hcw.cases',
      'cum.hcw.deaths')

    start.hcw.b <- 'Confirmed HCW'
    label.hcw.b <- c('cum.hcw.cases', 'cum.hcw.deaths')

    start.contact <- 'Newly reported' # Contact investigation summary
    label.contact <- c('new.contacts','under.followup','seen.on.day',
      'completed.21d.followup', 'lost.followup')

    start.case.c <- 'New Ebola Cases and Deaths Summarised by County'
    label.case.c <- c('alive.total','alive.suspect',
      'alive.probable','alive.confirm',
      'dead.total','dead.suspect','dead.probable',
      'dead.community','dead.patients')
    label.case.d <- c('alive.total','alive.suspect',
      'alive.probable', 'dead.total',
      'cum.total','cum.suspect','cum.probable','cum.confirm','cum.death')

    start.vhf <- 'VHF DATABASE'
    label.vhf <- c('cum.total','cum.suspect','cum.probable',
      'cum.confirm','cum.death')

    config.a <- list(min.version=175, max.version=196,
      sections=3, markers=markers,
      start.1=start.case.a, label.1=label.case.a,
      start.2=start.hcw, label.2=label.hcw,
      start.3=start.contact, label.3=label.contact)
    config.b <- list(min.version=197, max.version=197,
      sections=4, markers=markers,
      start.1=start.case.c, label.1=label.case.c,
      start.2=start.hcw, label.2=label.hcw,
      start.3=start.contact, label.3=label.contact,
      start.4=start.vhf, label.4=label.vhf, handler.4='vhf_fn')
    config.c <- list(min.version=198, max.version=198,
      sections=3, markers=markers,
      start.1=start.case.c, label.1=label.case.d,
      start.2=start.hcw, label.2=label.hcw,
      start.3=start.contact, label.3=label.contact)
    config.d <- list(min.version=199, max.version=200,
      sections=4, markers=markers,
      start.1=start.case.a, label.1=label.case.b,
      start.2=start.case.c, label.2=label.case.c,
      start.3=start.hcw, label.3=label.hcw,
      start.4=start.contact, label.4=label.contact)
    config.e <- list(min.version=201, max.version=MAX_VERSION,
      sections=4, markers=markers,
      start.1=start.case.a, label.1=label.case.b,
      start.2=start.case.c, label.2=label.case.c,
      start.3=start.hcw.b, label.3=label.hcw.b, handler.3='hcw_fn',
      start.4=start.contact, label.4=label.contact)

    config <- list(config.a, config.b, config.c, config.d, config.e)

    parts <- strsplit(file.name, '_')[[1]]
    version <- as.numeric(parts[2])
    truth <- sapply(config, function(cfg)
      version >= cfg$min.version && version <= cfg$max.version)
    out <- config[truth][[1]]
    out$date <- parse_date(file.name)
    out
  }

  #df1 <- row.fn(start.marker,'PAGE',markers,labels)
  #df2 <- row.fn(start.marker,'PAGE',markers,labels)
  #df3 <- row.fn(start.marker,'PAGE',markers,labels)
  #merge(merge(df1,df2, by='county', all=TRUE), df3, by='county', all=TRUE)

  regex <- list(
    list('^[[:space:]]+',''),
    list('[[:space:]]+$',''),
    list('Rivercess','River Cess'),
    list('Garpolu','Gbarpolu'),
    list('Grand Cape$','Grand Cape Mount'),
    list('^Mount$','')
  )

  config <- get_config(file.name)
  init <- data.frame(county=markers, date=config$date)
  fn <- function(i,acc) {
    start <- config[[sprintf('start.%s',i)]]
    label <- config[[sprintf('label.%s',i)]]
    handler <- 'get_rows'
    if (! is.null(config[[sprintf('handler.%s',i)]]))
      handler <- config[[sprintf('handler.%s',i)]]

    o <- do.call(handler, list(lines, start,'PAGE',config$markers,label,regex))
    merge(acc,o, by='county', all=TRUE)
  }
  out <- fold(1:config$sections, fn, init, simplify=FALSE)
  keep <- c('date','county', grep('^[^_]',colnames(out), value=TRUE))
  out[,keep]
}


#' Parse a date from a file name.
#'
#' This is for Liberia data
parse_date <- function(file.name) {
  file.name <- gsub('_+','_',file.name)
  parts <- strsplit(file.name, '_')[[1]]
  parts[4:5] <- gsub('[^0-9]','',parts[4:5])
  date.string <- sprintf('%s %s %s', parts[3], parts[4], parts[5])
  as.Date(strptime(date.string, '%b %d %Y'))
}

