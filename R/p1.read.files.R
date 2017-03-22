library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####
## 2017-02-23: MT4-EA/Trade raw tickets code optimize, parse data earlier catch.
## 2017-02-16: @DONE loose coupling for environment
## 2017-02-16: @DONE parallel problem, they should just return values

#### @PATCH NOTE@ ####
## 2017-03-21: NEW VERSION for Ubuntu & Optimize
## 2017-02-23: REPORT list include:
##              PHASE 1: c('INFOS', 'HTML.PARSE', 'PATH') 
##              PHASE 2: c('CURRENCY', 'LEVERAGE', 'TICKETS.RAW', 'ITEM.SYMBOL.MAPPING',
##                         'SUPPORTED.ITEM', 'UNSUPPORTED.ITEM', 'TICKETS.SUPPORTED') 
## 2017-02-22: Version 0.2 loose coupling for environment
## 2017-02-05: Version 0.1


#### HTML FILE TYPES EIGEN ####
HTML.FILE.TYPES.EIGEN <- c(
  'MT4-EA' = 'Strategy Tester:',
  'MT4-TRADE' = 'Statement:',
  'MT5-EA' = 'Strategy Tester Report',
  'MT5-TRADE' = 'Trade History Report',
  'MT4M-CLOSED' = 'Closed Trades Report',
  'MT4M-RAW' = 'Raw Report'
)


html.report.phase1 <- function(mq.file) {
  tryCatch(
    suppressWarnings(readLines(con8 <- file(mq.file, open = 'rt', encoding = 'UTF-8'), 4, ok = FALSE, warn = FALSE)),
    error = function(e) {
      lines <- readLines(con16 <- file(mq.file, open = 'rt', encoding = 'UTF-16'), 4, ok = FALSE, warn = FALSE)
      close(con16)
      lines
    },
    finally = close(con8)
  ) %>%
    extract((.) %>% str_detect('<title>') %>% which) %>%
    read_html %>%
    xml_find_first('.//title') %>%
    xml_text %>% {
      if (grepl(FILE.TYPES.EIGEN['MT4-EA'], .)) {
        html.parse <- read_html(mq.file, encoding = 'GBK')
        # infos <- fetch.html.data.infos.mt4ea(html.parse)
        list(
          INFOS = fetch.html.data.infos.mt4ea(html.parse)
        ) %>%
          append(fetch.html.data.others.mt4ea(html.parse))
      } else if (grepl(HTML.FILE.TYPES.EIGEN['MT4-TRADE'], .)) {
        html.parse <- read_html(mq.file, encoding = 'GBK')
        # infos <- fetch.html.data.infos.mt4trade(html.parse)
        list(
          INFOS = fetch.html.data.infos.mt4trade(html.parse)
        ) %>%
          append(fetch.html.data.others.mt4trade(html.parse))
      } else if (grepl(HTML.FILE.TYPES.EIGEN['MT5-EA'], .)) {
        # html.parse <- read_html(mq.file, encoding = 'UTF-16')
        # infos <- fetch.html.data.infos.mt5ea(html.parse)
        read_html(mq.file, encoding = 'UTF-16') %>% fetch.html.data.infos.mt5ea %>% list(INFOS = .)
      } else if (grepl(HTML.FILE.TYPES.EIGEN['MT5-TRADE'], .)) {
        # html.parse <- read_html(mq.file, encoding = 'UTF-16')
        # infos <- fetch.html.data.infos.mt5trade(html.parse)
        read_html(mq.file, encoding = 'UTF-16') %>% fetch.html.data.infos.mt5trade %>% list(INFOS = .)
      } else if (grepl(HTML.FILE.TYPES.EIGEN['MT4M-CLOSED'], .)) {
        # html.parse <- 'NEEDLESS'
        # infos <- fetch.html.data.infos.mt4m_closed()
        fetch.html.data.infos.mt4m_closed() %>% list(INFOS = .)
      } else if (grepl(HTML.FILE.TYPES.EIGEN['MT4M-RAW'], .)) {
        # html.parse <- 'NEEDLESS'
        # infos <- fetch.html.data.infos.mt4m_raw()
        fetch.html.data.infos.mt4m_raw() %>% list(INFOS = .)
      } else {
        NULL
      }
    }
}

#### PHASE1 ####
html.report.phase1


#### INFOS ####
INFOS.TABLE <- data.table(
  FILE = NA_character_,
  TYPE = NA_character_,
  ACCOUNT = NA_integer_,
  NAME = NA_character_,
  BROKER = NA_character_,
  CURRENCY = NA_character_,
  LEVERAGE = NA_integer_,
  TIME = as.POSIXct(NA, origin = '1970-01-01', tz = 'GMT')
)

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
  numeric.char <- str_replace_all(account, '[^[:digit:]]', '')
  ifelse(numeric.char == '', NA_integer_, as.numeric(numeric.char))
}

format.infos.name <- function(name) {
  # ''' format report info: name '''
  # 2017-01-16: Version 0.1
  name.char <- gsub('Name: ', '', name)
  ifelse(name.char == '', NA_character_, name.char)
} # FINISH

format.infos.broker <- function(broker) {
  # ''' format report info: broker '''
  # 2017-01-16: Version 0.1
  broker.char <- gsub(' .*', '', broker)
  ifelse(broker.char == '', NA_character_, broker.char)
} # FINISH

format.infos.currency <- function(currency) {
  # ''' format report info: currency '''
  # 2017-01-16: Version 0.1
  currency.char <-
    gsub('Currency: ', '', currency) %>%
    str_extract('[[:upper:]]+')
  ifelse(currency.char == '', NA_character_, currency.char)
} # FINISH

format.infos.leverage <- function(leverage) {
  # ''' format report info: leverage '''
  # 2017-01-16: Version 0.1
  leverage.char <-
    gsub('1:', '', leverage) %>%
    str_extract('[[:digit:]]+')
  ifelse(leverage.char == '', NA_integer_, as.numeric(leverage.char))
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
  ydm_hm(time, tz = 'GMT')
} # FINISH

time.numeric.to.posixct <- function(time) {
  as.POSIXct(time, origin = '1970-01-01', tz = 'GMT')
} # FINISH


#### OTHERS ####
fetch.html.data.others.mt4ea <- function(mq.file.parse) {
  xml.text <- mq.file.parse %>% xml_find_first('.//table') %>% xml_find_all('.//td') %>% xml_text
  time.string <- xml.text[4]
  len.time.string <- nchar(time.string)
  list(
    .DEPOSIT = xml.text %>% extract(24) %>% as.numeric,
    .DEPOSIT.TIME = time.string %>% substr(len.time.string - 23, len.time.string - 14) %>% format.time.all.to.numeric,
    .END.TIME = time.string %>% substr(len.time.string - 10, len.time.string - 1) %>% format.time.all.to.numeric,
    .ITEM = xml.text %>% extract(2) %>% gsub(' ([ \\(\\)[:alpha:]])*', '', .)
  )
}

fetch.html.data.others.mt4trade <- function(mq.file.parse) {
  list(
    .COMMENT = xml_find_first(mq.file.parse, './/table') %>% xml_find_all('.//tr') %>% xml_find_first('.//td') %>%
      xml_attr('title', default = '') %>% extract(-1)
  )
}
