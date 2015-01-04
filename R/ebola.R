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

require(lambda.tools)
require(futile.logger)
require(RCurl)
require(scrapeR)
require(httr)

flog.threshold(DEBUG)


LR_DIR <- './data/lr'
LR_SITE <- 'http://www.mohsw.gov.lr/content_display.php?sub=report2'

SL_DIR <- './data/sl'
SL_SITE <- 'http://health.gov.sl/?page_id=583'



replace_ext <- function(file, ext) {
  parts <- strsplit(file, ".", fixed=TRUE)
  sapply(parts, function(x) paste(c(x[1:(length(x)-1)], ext), collapse='.'))
}

# Get file names of parsed files
lr_files <- function(base=LR_DIR) {
  list.files(base, 'SITRep.*\\.txt')
}

sl_files <- function(base=SL_DIR) {
  list.files(base, 'Ebola-.*\\.txt')
}

do_xml <- function(x, pattern) {
  link <- xmlAttrs(x)['href']
  link <- grep(pattern,link, value=TRUE)
  gsub(' ','%20',link, fixed=TRUE)
}

do_etl <- function(url, file, base) {
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

extract_sl <- function(url=SL_SITE, base=SL_DIR) {
  page <- scrape(url)
  xpath <- "//div[@class='post']/ul/li/strong/a"
  pat <- 'Situation-Report'
  urls <- do.call(c, 
    xpathSApply(page[[1]], xpath, function(x) do_xml(x,pat), simplify=FALSE))
  files <- sapply(strsplit(urls,'/'), function(x) gsub('%20','_',x[length(x)]))

  apply(cbind(urls,files), 1, function(u) do_etl(u[1], u[2], base))
  o <- replace_ext(files,'txt')
  names(o) <- NULL
  o
}

parse_date <- function(file.name) {
  file.name <- gsub('_+','_',file.name)
  parts <- strsplit(file.name, '_')[[1]]
  parts[4:5] <- gsub('[^0-9]','',parts[4:5])
  date.string <- sprintf('%s %s %s', parts[3], parts[4], parts[5])
  as.Date(strptime(date.string, '%b %d %Y'))
}

do_extract <- function(lines, start.marker, stop.marker, regex) {
  start.idx <- grep(start.marker,lines)
  if (length(start.idx) < 1) 
    flog.warn("[do_extract] No match for start marker %s",start.marker)
  stop.idx <- grep(stop.marker,lines[(1+start.idx):length(lines)])[1]
  if (is.na(stop.idx)) stop.idx <- length(lines) - start.idx
  
  chunk <- lines[start.idx:(start.idx+stop.idx)]
  chunk <- fold(regex, function(r,ch) sub(r[[1]],r[[2]],ch), chunk)
  grep('^$',chunk, invert=TRUE, value=TRUE)
}

get_index <- function(raw.table, markers) {
  section.idx <- lapply(markers, function(x) grep(sprintf('^%s$',x), raw.table))
  # TODO: Add check for NULL
  do.call(c, section.idx)
}

get_series <- function(raw.table, index, size) {
  chunk <- raw.table[(index+1):(index+size)]
  as.numeric(gsub('[^0-9]','', chunk))
}

get_cols <- function(lines, start.marker, stop.marker, markers, labels, regex) {
  size <- length(labels)
  raw.table <- do_extract(lines, start.marker, stop.marker, regex)
  indices <- get_index(raw.table, markers)
  columns <- lapply(indices, function(i) get_series(raw.table,i,size))
  names(columns) <- raw.table[indices]
  as.data.frame(columns)
}

get_rows <- function(lines, start.marker, stop.marker, markers, labels, regex) {
  size <- length(labels)
  raw.table <- do_extract(lines, start.marker, stop.marker, regex)
  indices <- get_index(raw.table, markers)
  rows <- lapply(indices, function(i) get_series(raw.table,i,size))
  df <- as.data.frame(do.call(rbind, rows))
  colnames(df) <- labels
  df$county <- sub('[[:space:]]*$', '', raw.table[indices])
  df
}

# fs <- extract_lr()
# data <- parse_all(fs, 'lr')
# with(data[data$county=='NATIONAL',], plot(date,alive.total,type='l'))
parse_all <- function(fs, type='lr') {
  o <- lapply(fs, function(f) parse_file(f, type))
  cols <- fold(o, function(i, acc) union(acc,colnames(i)), c())
  o <- lapply(o, function(x) { x[,setdiff(cols,colnames(x))] <- NA; x })
  o <- do.call(rbind, o)
  o[order(o$date,o$county),]
}

parse_file <- function(file.name, type) {
  if (! type %in% c('lr','sl')) stop("Incorrect type")

  flog.info("[%s] Process %s", type, file.name)
  cmd <- sprintf('parse_%s',type)
  do.call(cmd, list(file.name))
}


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


## Parse Sierra Leone
parse_sl <- function(file.name, base=SL_DIR) {
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


# Use as filter example
# Sum of each series over county should equal national
# validate(data,'alive.total')
validate <- function(data, column, nation='NATIONAL') {
  dates <- unique(data$date)
  day_fn <- function(d) {
    day <- data[data$date==d,]
    counties <- sum(day[day$county!=nation,column])
    national <- day[day$county==nation,column]
    c(counties=counties, national=national)
    #counties == national
  }
  #debug(day_fn)
  o <- t(sapply(dates, day_fn))
  rownames(o) <- format(dates)
  o
}

# This is designed for Sierra Leone data, which is a bit more consistent
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
