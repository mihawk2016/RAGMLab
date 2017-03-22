library(compiler)
compilePKGS(T)

#### @PATCH NOTE@ ####
## 2017-02-12: Version 0.1

#### INFOS TABLE ####
INFOS.COLUMNS <- c('FILE', 'TYPE', 'ACCOUNT', 'NAME', 'BROKER', 'CURRENCY', 'LEVERAGE', 'TIME')
INFOS.TABLE <-
  data.table(
    matrix(ncol = length(INFOS.COLUMNS), dimnames = list(NULL, INFOS.COLUMNS))
  )

#### FETCH INFOS ####
fetch.html.data.infos.mt4ea <- function(mq.file.parse) {
  
  head.lines <- xml_text(xml_find_all(mq.file.parse, '//b')[2:3])
  first.table <- xml_find_first(mq.file.parse, '//table')
  time.string <- xml_text(xml_find_all(first.table, '//td')[4])
  nchar.time.string <- nchar(time.string)
  copy(INFOS.TABLE) %>%
    extract(
      j = c('TYPE', 'NAME', 'BROKER', 'TIME') :=
        list('MT4-EA',
             format.infos.name(head.lines[1]),
             format.infos.broker(head.lines[2]),
             format.infos.time(substr(time.string, nchar.time.string - 10, nchar.time.string - 1)))
    )
} # FINISH

fetch.html.data.infos.mt4trade <- function(mq.file.parse) {
  
  first.row <- xml_text(xml_find_all(xml_find_first(xml_find_first(mq.file.parse, '//table'), './/tr'), './/b'))
  copy(INFOS.TABLE) %>%
    extract(
      j = c('TYPE', 'ACCOUNT', 'NAME', 'BROKER', 'CURRENCY', 'LEVERAGE', 'TIME') :=
        list('MT4-Trade',
             format.infos.account(first.row[grep('Account', first.row)]),
             format.infos.name(first.row[grep('Name', first.row)]),
             format.infos.broker(xml_text(xml_find_first(mq.file.parse, '//b'))),
             format.infos.currency(first.row[grep('Currency', first.row)]),
             format.infos.leverage(first.row[grep('Leverage', first.row)]),
             format.infos.time(tail(first.row, 1)))
    )
} # FINISH

fetch.html.data.infos.mt5ea <- function(mq.file.parse) {
  
  table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/td'))
  time.string <- table.values[grep('Period:', table.values) + 1]
  nchar.time.string <- nchar(time.string)
  copy(INFOS.TABLE) %>%
    extract(
      j = c('TYPE', 'NAME', 'BROKER', 'CURRENCY', 'LEVERAGE', 'TIME') :=
        list('MT5-EA',
             format.infos.name(table.values[grep('Expert:', table.values) + 1]),
             format.infos.broker(table.values[grep('Broker:', table.values) + 1]),
             format.infos.currency(table.values[grep('Currency:', table.values) + 1]),
             format.infos.leverage(table.values[grep('Leverage:', table.values) + 1]),
             format.infos.time(substr(time.string, nchar.time.string - 10, nchar.time.string - 1)))
    )
} # FINISH

fetch.html.data.infos.mt5trade <- function(mq.file.parse) {
  
  table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/th'))
  account.currency.leverage <- table.values[grep('Account:', table.values) + 1]
  copy(INFOS.TABLE) %>%
    extract(
      j = c('TYPE', 'ACCOUNT', 'NAME', 'BROKER', 'CURRENCY', 'LEVERAGE', 'TIME') :=
        list('MT5-Trade',
             format.infos.account(account.currency.leverage),
             format.infos.name(table.values[grep('Name:', table.values) + 1]),
             format.infos.broker(table.values[grep('Broker:', table.values) + 1]),
             format.infos.currency(account.currency.leverage),
             format.infos.leverage(account.currency.leverage),
             format.infos.time(format.infos.time(table.values[grep('Date:', table.values) + 1]) - 8 * 3600))
    )
} # FINISH

fetch.html.data.infos.mt4m_closed <- function() {
  copy(INFOS.TABLE) %>%
    extract(j = TYPE := 'MT4M-Closed')
} # FINISH

fetch.html.data.infos.mt4m_raw <- function() {
  copy(INFOS.TABLE) %>%
    extract(j = TYPE := 'MT4M-Raw')
} # FINISH

#### FORMAT ####
format.infos.account <- function(account) {
  if (is.na(account) || is.numeric(account)) {
    return(account)
  }
  if (account == '') {
    return(NA)
  }
  if (is.character(account)) {
    account <- gsub('Account: ', '', account)
    match1 <- regexpr('[[:digit:]]*', account)
    if (match1) {
      account <- substr(account, match1, attr(match1, 'match.length') + match1 - 1)
    }
    return(as.numeric(account))
  }
  NA
}

format.infos.name <- function(name) {
  # ''' format report info: name '''
  # 2017-01-16: Version 0.1
  if (is.na(name)) {
    return(name)
  }
  name <- gsub('Name: ', '', name)
  ifelse(name == '', NA, name)
} # FINISH

format.infos.broker <- function(broker) {
  # ''' format report info: broker '''
  # 2017-01-16: Version 0.1
  if (is.na(broker)) {
    return(broker)
  }
  gsub(' .*', '', broker)
} # FINISH

format.infos.currency <- function(currency) {
  # ''' format report info: currency '''
  # 2017-01-16: Version 0.1
  if (is.na(currency)) {
    return(currency)
  }
  currency <- gsub('Currency: ', '', currency)
  match1 <- regexpr('[[:upper:]]+', currency)
  if (match1) {
    currency <- substr(currency, match1, attr(match1, 'match.length') + match1 - 1)
  }
  ifelse(currency == '', NA, currency)
} # FINISH

format.infos.leverage <- function(leverage) {
  # ''' format report info: leverage '''
  # 2017-01-16: Version 0.1
  if (is.na(leverage) | is.numeric(leverage)) {
    return(leverage)
  }
  if (leverage == '') {
    return(NA)
  }
  if (is.character(leverage)) {
    match1 <- regexpr('1:[[:digit:]]+', leverage)
    if (match1) {
      leverage <- substr(leverage, match1 + 2, attr(match1, 'match.length') + match1 - 1)
    }
    return(as.numeric(leverage))
  }
  NA
} # FINISH

format.infos.time <- function(time) {
  # ''' format report info: time '''
  # 2017-01-16: Version 0.1
  format.time.all.to.numeric(time)
} # FINISH

format.time.all.to.numeric <- function(time) {
  if (is.na(time) || is.numeric(time)) {
    return(time)
  }
  if (is.character(time)) {
    if (grepl(',', time)) {
      time <- format.mt4trade.infos.time(time)
    } else {
      time <- gsub('-', '.', time)
      format <- '%Y.%m.%d %H:%M:%S'
      sub_format <- substr(format, 1, nchar(time) - 2)
      time <- as.POSIXct(time, format = sub_format, tz = 'GMT')
    }
    return(as.numeric(time))
  }
  NA
} # FINISH

format.mt4trade.infos.time <- function(time) {
  # ''' format mt4trade info time '''
  # 2016-08-16: Version 1.0
  os <- Sys.info()['sysname']
  if (os == 'Windows') {
    Sys.setlocale(locale = 'us')
    new_time <- as.POSIXct(time, '%Y %b %d, %H:%M', tz = 'GMT')
    Sys.setlocale(locale = 'Chinese')
  } else if (os == 'Linux') {
    Sys.setlocale(locale = 'en_US.UTF-8')
    new_time <- as.POSIXct(time, '%Y %b %d, %H:%M', tz = 'GMT')
    Sys.setlocale(locale = 'zh_CN.UTF-8')
  } else {
    return(NULL)
  }
  new_time
} # FINISH

time.numeric.to.posixct <- function(time) {
  as.POSIXct(time, origin = '1970-01-01', tz = 'GMT')
} # FINISH
