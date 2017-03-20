library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####
## 2017-03-02: try posixct time in table; add symbol - calendars for shiny; Sys.setlocale() for whole report
##             use more environment for timeseries; max floating profit time & tickets period need to minus weekend time
## 2017-02-20: mysql.price.open add parallel method

#### @PATCH NOTE@ ####
## 2017-02-13: Version 0.2 speed of mysql is good enough, hang-up the local method
## 2017-02-13: Version 0.1

MYSQL.SETTING <- list(
  HOST = '192.168.2.107',
  PORT = 3306,
  USERNAME = 'root',
  PASSWORD = '',
  DBNAME = 'historical_data'
)

#### DATABASE FUNCTIONS ####
DB.O <- function(symbol, time, mysql.setting=MYSQL.SETTING, timeframe='M1') {
  mysql.price.open(symbol, time, mysql.setting, timeframe)
}

DB.OHLC <- function(symbol, from, to, timeframe='H1', mysql.setting=MYSQL.SETTING, cluster=NULL) {
  mysql.price.ohlc(symbol, from, to, timeframe, mysql.setting, cluster)
}

#### MYSQL ####
mysql.query <- function(sql, setting=MYSQL.SETTING, cluster=NULL) {
  # ''' mysql query '''
  # 2017-01-22: Version 1.0
  if (!length(sql)) {
    return(NULL)
  }
  mysql.connection <- tryCatch(
    dbConnect(MySQL(), host = setting$HOST, port = setting$PORT, username = setting$USERNAME, password = setting$PASSWORD, dbname = setting$DBNAME),
    error = function(e) {
      message('MySQL Connect ERROR')
      NULL
    }
  )
  if (is.null(mysql.connection)) {
    return(NULL)
  }
  res <-
    if (is.null(cluster) | length(sql) < 4) {
      lapply(sql, mysql.get.data.table, mysql.connection)
    } else {
      parLapply(cluster, sql, mysql.get.data.table, mysql.connection)
    }
  dbDisconnect(mysql.connection)
  res
} # FINISH

mysql.get.data.table <- function(sql, connection) {
  dbGetQuery(connection, sql) %>%
    as.data.table
} # FINISH

mysql.price.open <- function(symbol, time, timeframe='M1', setting=MYSQL.SETTING) {
  # ''' get open data from mysql database '''
  # 2017-02-16: Version 2.0 use data.table, optimize method
  # 2017-01-22: Version 1.0
  table <-
    paste(symbol, timeframe, sep = '_') %>%
    tolower
  time.table <- data.table(time = time, key = 'time')
  max.time <-
    max(time) %>%
    time.numeric.to.posixct %>%
    as.Date %>%
    add(1)
  min.time <-
    min(time) %>%
    time.numeric.to.posixct %>%
    as.Date
  time.period <- gsub('-', '.', as.character(c(min.time, max.time)))
  sql.string <- "SELECT time, open FROM %s WHERE time BETWEEN '%s' AND '%s'"
  sql <- sprintf(sql.string, table, time.period[1], time.period[2])
  mysql.query(sql, setting)[[1]] %>%
    as.data.table %>%
    extract(j = time := time.char.to.num(time)) %>%
    setkey(time) %>%
    extract(i = time.table, roll = 'nearest') %>%
    extract(j = open)
} # FINISH

mysql.price.ohlc <- function(symbol, from, to, timeframe='H1', setting=MYSQL.SETTING, cluster=NULL) {
  # ''' get ohlc from mysql database '''
  # 2017-01-22: Version 1.0
  table <-
    paste(symbol, timeframe, sep = '_') %>%
    tolower
  from %<>%
    time.numeric.to.posixct %>%
    as.Date
  to %<>%
    time.numeric.to.posixct %>%
    as.Date %>%
    add(1)
  time.period <- gsub('-', '.', as.character(c(from, to)))
  sql.string <- "SELECT time, open, high, low, close FROM %s WHERE time BETWEEN '%s' AND '%s'"
  sql <- sprintf(sql.string, table, time.period[1], time.period[2])
  mysql.query(sql, setting) %>%
    set_names(symbol) %>%
    lapply(function(price.table) {
      price.table[j = time := time.char.to.num(time)] %>%
        setnames(toupper(names(.))) %>%
        setkey(TIME)
    })
} # FINISH


# 
# #### LOCAL ENVIRONMENT ####
# init.local.data <- function() {
#   assign('LOCAL.DATA', NULL, envir = METAQUOTE.ANALYSTIC)
# }
# 
# get.local.data <- function() {
#   get('LOCAL.DATA', envir = METAQUOTE.ANALYSTIC)
# }
# 
# set.local.data <- function(local.data) {
#   METAQUOTE.ANALYSTIC$LOCAL.DATA <- local.data
# }
# 
# get.local.data.table <- function(symbol, timeframe) {
#   get('LOCAL.DATA', envir = METAQUOTE.ANALYSTIC) %>%
#     extract2(symbol) %>%
#     extract2(timeframe)
# }
# 
# set.local.data.table <- function(symbol, timeframe, data) {
#   symbols <-
#     get.local.data() %>%
#     names
#   if (!symbol %in% symbols) {
#     METAQUOTE.ANALYSTIC$LOCAL.DATA[[symbol]] <- list()
#   }
#   METAQUOTE.ANALYSTIC$LOCAL.DATA[[symbol]][[timeframe]] <- data
# }
# 
# set.local.file.path <- function(file) {
#   METAQUOTE.ANALYSTIC$local.file <- file
# }
# 
# get.local.file.path <- function() {
#   METAQUOTE.ANALYSTIC$local.file
# }
# 
# #### LOCAL ####
# load.local.file <- function(file) {
#   set.local.file.path(file)
#   if (!file.exists(file)) {
#     file.create(file)
#     set.local.data(build.local.data.tree())
#     return(NULL)
#   }
#   tryCatch(
#     set.local.data(get(load(file))),
#     error = function(e) {
#       message('Local Data File ERROR')
#       NULL
#     }
#   )
# }
# 
# save.local.file = function(data, file) {
#   if (missing(file)) {
#     file <- get.local.file.path()
#   }
#   tryCatch(
#     save(data, file = file),
#     error = function(e)  {
#       message('Local Data File ERROR')
#       NULL
#     }
#   )
# }
# 
# build.local.data.tree <- function(support.symbols=SYMBOLS.SETTING[, SYMBOL], timeframes=TIMEFRAMES) {
#   timeframe.list <-
#     vector('list', length(timeframes)) %>%
#     set_names(timeframes)
#   symbol.list <-
#     vector('list', length(support.symbols)) %>%
#     set_names(support.symbols)
#   lapply(symbol.list, function(symbol) timeframe.list)
# } # FINISH
# 
# read.data.csv <- function(file.csv) {
#   csv <- fread(file.csv)
#   new.csv <-
#     data.table(
#       ymd_hm(paste(csv$V1, csv$V2), tz = 'GMT'),
#       csv[j = c(3:7)]
#     ) %>%
#     as.xts.data.table %>%
#     set_colnames(c('Open', 'High', 'Low', 'Close', 'Volume'))
# } # FINISH
# 
# import.csv = function(file.csv) {
#   if (is.data.frame(file.csv)) {
#     file.name <- file.csv$name
#   } else {
#     file.name <- basename(file.csv)
#   }
#   split.file.name <- strsplit(file.name, '_')[[1]]
#   symbol <- split.file.name[1]
#   timeframe <- split.file.name[2]
#   new.data <- read.data.csv(file.csv)
#   old.data <- get.local.data.table(symbol, timeframe)
#   merged.data <- rbind(old.data, new.data)
#   merged.data <- merged.data[!duplicated(index(merged.data))] 
#   set.local.data.table(symbol, timeframe, merged.data)
# } # FINISH

