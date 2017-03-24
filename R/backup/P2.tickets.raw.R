



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
    TICKETS.RAW <- tickets.raw(infos[, TYPE], file, parse, CURRENCY, get.open.fun, mysql.setting, timeframe, symbols.setting)%>%
      extract(j = c('FILE.INDEX', 'FILE') := list(index, infos[, FILE]))
    
    ls(all.names = TRUE) %>% extract()
  })
}











