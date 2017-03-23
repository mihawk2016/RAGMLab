



generate.html.tickets <- function(file, parse, infos, index,
                                  default.currency=DEFAULT.CURRENCY, default.leverage=DEFAULT.LEVERAGE,
                                  get.open.fun=DB.O, mysql.setting=MYSQL.SETTING,
                                  timeframe='M1', symbols.setting=SYMBOLS.SETTING,
                                  parallel=PARALLEL.THRESHOLD.GENERATE.TICKETS) {
  if (is.numeric(parallel)) {
    parallel <- length(file) >= parallel
  }
  if (!parallel) {
    phase2data <- mapply(fetch.html.tickets, file, parse, infos, index,
                         MoreArgs = list(default.currency, default.leverage, get.open.fun, mysql.setting,
                                         timeframe, symbols.setting), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else {
    cluster <- makeCluster(detectCores() - 1)
    phase2data <- clusterMap(cluster, fetch.html.tickets, file, parse, infos, index,
                             MoreArgs = list(default.currency, default.leverage, get.open.fun, mysql.setting,
                                             timeframe, symbols.setting), SIMPLIFY = FALSE, USE.NAMES = FALSE)
    stopCluster(cluster)
  }
  phase2data
}

reports.PHASE2 <- function(report, index,
                           default.currency=DEFAULT.CURRENCY, default.leverage=DEFAULT.LEVERAGE, get.open.fun=DB.O,
                           mysql.setting=MYSQL.SETTING, timeframe='M1', symbols.setting=SYMBOLS.SETTING,
                           parallel=PARALLEL.THRESHOLD.GENERATE.TICKETS) {
  if (is.numeric(parallel)) {
    parallel <- length(report) >= parallel
  }
  if (!parallel) {
    phase2data <- mapply(fetch.html.tickets2, report, index,
                         MoreArgs = list(default.currency, default.leverage, get.open.fun, mysql.setting, timeframe, symbols.setting),
                         SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else {
    cluster <- makeCluster(detectCores() - 1)
    phase2data <- clusterMap(cluster, fetch.html.tickets2, report, index,
                             MoreArgs = list(default.currency, default.leverage, get.open.fun, mysql.setting, timeframe, symbols.setting),
                             SIMPLIFY = FALSE, USE.NAMES = FALSE)
    stopCluster(cluster)
  }
  phase2data
}

#### FETCH TICKETS ####
fetch.html.tickets <- function(file, parse, infos, index, default.currency=DEFAULT.CURRENCY, default.leverage=DEFAULT.LEVERAGE,
                               get.open.fun=DB.O, mysql.setting=MYSQL.SETTING, timeframe='M1', symbols.setting=SYMBOLS.SETTING) {
  within(list(
    PATH = file,
    HTML.PARSE = parse,
    INFOS = infos
  ), {
    CURRENCY <- report.currency(infos, default.currency)
    LEVERAGE <- report.leverage(infos, default.leverage)
    TICKETS.RAW <- tickets.raw(infos[, TYPE], file, parse, CURRENCY, get.open.fun, mysql.setting, timeframe, symbols.setting)%>%
      extract(j = c('FILE.INDEX', 'FILE') := list(index, infos[, FILE]))
    ITEM.SYMBOL.MAPPING <- item.symbol.mapping(TICKETS.RAW, symbols.setting[, SYMBOL])
    SUPPORTED.ITEM <- supported.items(ITEM.SYMBOL.MAPPING)
    UNSUPPORTED.ITEM <- unsupported.items(ITEM.SYMBOL.MAPPING)
    TICKETS.SUPPORTED <- tickets.supported(TICKETS.RAW, ITEM.SYMBOL.MAPPING)
    TICKETS.EDITING <- tickets.editing(TICKETS.SUPPORTED)
    PHASE <- 2
  })
}

fetch.html.tickets2 <- function(report, index, default.currency=DEFAULT.CURRENCY, default.leverage=DEFAULT.LEVERAGE,
                                get.open.fun=DB.O, mysql.setting=MYSQL.SETTING, timeframe='M1', symbols.setting=SYMBOLS.SETTING) {
  with(report, {
    fetch.html.tickets(PATH, HTML.PARSE, INFOS, index, default.currency, default.leverage, get.open.fun, mysql.setting, timeframe, symbols.setting)
  })
}










# #### UTILS ####
# bind.comment.and.login <- function(comment, login) {
#   ifelse(comment == '' || is.na(comment), login, paste(comment, login, sep = ' | '))
# }
#### BUILD TICKETS ####


#### CALCULATION ####


#### REPORT SETTINGS ####
report.currency <- function(infos, default=DEFAULT.CURRENCY) {
  info.currency <-
    infos[, CURRENCY] %>%
    unique
  if (length(info.currency) > 1) {
    return(default)
  }
  ifelse(is.na(info.currency), default, info.currency)
}

report.leverage <- function(infos, default=DEFAULT.LEVERAGE) {
  info.leverage <-
    infos[, LEVERAGE] %>%
    unique
  if (length(info.leverage) > 1) {
    return(default)
  }
  ifelse(is.na(info.leverage), default, info.leverage)
}

