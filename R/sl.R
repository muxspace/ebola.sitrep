# :vim set ft=R

.SL_DIR <- './data/sl'
.SL_SITE <- 'http://health.gov.sl/?page_id=583'


#' Get file names of downloaded and converted files for Sierra Leone
#'
#' Once files are extracted from the source, they are saved locally.
#' This function reads the local directory and returns the names of 
#' all files to parse and consolidate into a single data.frame.
#'
#' @name files_sl
#' @param base The base directory to look for data files
#' @return A list of files
#' @author Brian Lee Yung Rowe
#' @examples
#' \dontrun{
#' x <- files_sl()
#' }
files_sl <- function(base=.SL_DIR) {
  list.files(base, 'Ebola-.*\\.txt')
}


#' Download situation reports from the Sierra Leone Ministry of Health
#'
#' Extract all report links from the Ministry of Health web site and
#' download each situation report locally.
#'
#' This function also converts each PDF into a text file and does
#' preprocessing to simplify parsing.
#' @name extract_sl
#' @param url The URL of the list of situation reports
#' @param base The base directory to save data files
#' @return The list of local file names
#' @author Brian Lee Yung Rowe
#' @examples
#' \dontrun{
#' fs <- extract_sl()
#' intersect(fs, files_sl())
#' }
extract_sl <- function(url=.SL_SITE, base=.SL_DIR) {
  page <- scrape(url)
  xpath <- "//div[@class='post']/ul/li/strong/a"
  pat <- 'Situation-Report'
  urls <- do.call(c, 
    xpathSApply(page[[1]],xpath,function(x) find_urls(x,pat), simplify=FALSE))
  files <- sapply(strsplit(urls,'/'), function(x) gsub('%20','_',x[length(x)]))

  apply(cbind(urls,files), 1, function(u) extract_pdf(u[1], u[2], base))
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
#' @name parse_sl
#' @param file.name The name of the situation report
#' @param base The base directory to look for the situation report
#' @return A data.frame representing thea situation report
#' @author Brian Lee Yung Rowe
#'
#' @examples
#' \dontrun{
#' fs <- files_sl()
#' report <- parse_sl(fs[1])
#' }
parse_sl <- function(file.name, base=.SL_DIR) {
  lines <- readLines(sprintf('%s/%s',base,file.name), warn=FALSE)
  flog.info("[sl] Found %s raw lines", length(lines))
  if (length(lines) < 1) return(NULL)

  date.line <- grep('EBOLA[[:space:]]+VIRUS DISEASE',lines, value=TRUE)
  date.line <- sub('^.*Sit-Rep  ','',date.line)
  date <- as.Date(strptime(date.line, '%d %B %Y'))

  markers <- c('Kailahun','Kenema','Kono','Kambia','Koinadugu',
    'Bombali','Tonkolili','Port Loko','Pujehun','Bo','Moyamba',
    'Bonthe','Western area urban','Western area rural','National')


  contacts_fn <- function(lines,start.marker,stop.marker,markers,labels,regex) {
    lines <- grep('^24h',lines, invert=TRUE,value=TRUE)
    lines <- grep('^in last',lines, invert=TRUE,value=TRUE)
    ms <- c('Followed','added in','healthy','ill in last','not seen',
      'last 24h','Tracers')
    df <- get_cols(lines, start.marker, stop.marker, ms, markers, regex)
    colnames(df) <- labels
    df$county <- markers
    df$tracers.on.day <- df$tracers.on.day/100
    df
  }

  get_config <- function(file.name) {
    MAX_VERSION <- 299

    start.case.a <- 'Non-Case Suspected Probable Confirmed Suspected'
    label.case.a <- c('population',
      'alive.non.case', 'alive.suspect','alive.probable','alive.confirm',
      'cum.non.case','cum.suspect','cum.probable','cum.confirm',
      'cum.dead.suspect','cum.dead.probable','cum.dead.confirmed', 'CFR')

    start.contact.a <- 'Summary of Contacts being  followed-up'
    label.contact.a <- c('total.contacts','new.on.day','healthy.on.day',
      'ill.on.day','unseen.on.day','completed.21d.followup','tracers.on.day')

    start.contact.b <- '[Ss]ummary of contacts followed up'
    label.contact.b <- c('total.contacts','completed.21d.total',
      'under.followup','new.on.day','healthy.on.day','ill.on.day',
      'unseen.on.day','completed.21d.followup','tracers.on.day')

    label.contact.c <- c('total.contacts','completed.21d.total',
      'incomplete.dead',
      'under.followup','new.on.day','healthy.on.day','ill.on.day',
      'unvisited.on.day',
      'unseen.on.day','completed.21d.followup','tracers.on.day')

    config.a <- list(min.version=77, max.version=93,
      sections=2, markers=markers,
      start.1=start.case.a, label.1=label.case.a, stop.1='Table 2',
      start.2=start.contact.a, label.2=label.contact.a, handler.2=contacts_fn)
    config.b <- list(min.version=94, max.version=190,
      sections=2, markers=markers,
      start.1=start.case.a, label.1=label.case.a,
      start.2=start.contact.b, label.2=label.contact.b)
    config.c <- list(min.version=191, max.version=202,
      sections=2, markers=markers,
      start.1=start.case.a, label.1=label.case.a,
      start.2=start.contact.b, label.2=label.contact.b)
    config.d <- list(min.version=203, max.version=MAX_VERSION,
      sections=2, markers=markers,
      start.1=start.case.a, label.1=label.case.a,
      start.2=start.contact.b, label.2=label.contact.c, stop.2='EOF')

    config <- list(config.a, config.b, config.c, config.d)

    parts <- strsplit(file.name, '[_\\.-]')[[1]]
    version <- as.numeric(parts[5])
    truth <- sapply(config, function(cfg)
      version >= cfg$min.version && version <= cfg$max.version)
    out <- config[truth][[1]]
    out$date <- date
    out
  }

  regex <- list(
    list('^[[:space:]]+',''),
    list('[[:space:]]+$',''),
    list('^Urban$','Western area urban'),
    list('^Rural$','Western area rural'),
    list('^Western Area$','')
  )

  config <- get_config(file.name)
  init <- data.frame(county=markers, date=config$date)
  fn <- function(i,acc) {
    start <- config[[sprintf('start.%s',i)]]
    label <- config[[sprintf('label.%s',i)]]
    handler <- 'get_rows'
    if (! is.null(config[[sprintf('handler.%s',i)]]))
      handler <- config[[sprintf('handler.%s',i)]]
    stop <- 'PAGE'
    if (! is.null(config[[sprintf('stop.%s',i)]]))
      stop <- config[[sprintf('stop.%s',i)]]

    tryCatch({
      o <- do.call(handler, list(lines, start,stop,config$markers,label,regex))
      merge(acc,o, by='county', all=TRUE)
    }, error=function(e) {
      flog.warn("Skipping bad parse section: %s",e)
      acc
    })
  }

  out <- fold(1:config$sections, fn, init, simplify=FALSE)
  if ('CFR' %in% colnames(out)) out$CFR <- out$CFR / 10
  keep <- c('date','county', grep('^[^_]',colnames(out), value=TRUE))
  out[,keep]
}

