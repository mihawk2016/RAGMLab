library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####
## 2017-02-22:timeserie.account optimize, table / len 

#### @PATCH NOTE@ ####
## 2017-02-17: Version 0.1

## additional tickets column: PIP, NPROFIT, PL, LOT.PROFIT
reports.PHASE3 <- function(report.phase2,
                           set.init.money=NULL, include.middle=TRUE, default.money=DEFAULT.INIT.MONEY,
                           currency=DEFAULT.CURRENCY, get.open.fun=DB.O, timeframe.tickvalue='M1',
                           get.ohlc.fun=DB.OHLC, timeframe.report='H1', parallel.db=PARALLEL.THRESHOLD.DB.SYMBOLS,
                           margin.base=1500, symbols.setting=SYMBOLS.SETTING, parallel.tickets=PARALLEL.THRESHOLD.GENERATE.TICKETS,
                           mysql.setting=MYSQL.SETTING) {
  if (is.numeric(parallel.tickets)) {
    parallel.tickets <- length(report.phase2) >= parallel.tickets
  }
  if (!parallel.tickets) {
    phase3data <- mapply(report.PHASE3, report.phase2,
                         MoreArgs = list(set.init.money, include.middle, default.money, currency, get.open.fun, timeframe.tickvalue,
                                         get.ohlc.fun, timeframe.report, parallel.db, margin.base, symbols.setting, mysql.setting),
                         SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else {
    cluster <- makeCluster(detectCores() - 1)
    phase3data <- clusterMap(cluster, report.PHASE3, report.phase2,
                             MoreArgs = list(set.init.money, include.middle, default.money, currency, get.open.fun, timeframe.tickvalue,
                                             get.ohlc.fun, timeframe.report, parallel.db, margin.base, symbols.setting, mysql.setting),
                             SIMPLIFY = FALSE, USE.NAMES = FALSE)
    stopCluster(cluster)
  }
  phase3data
}

report.PHASE3 <- function(report.phase2,
                          set.init.money=NULL, include.middle=TRUE, default.money=DEFAULT.INIT.MONEY,
                          currency=DEFAULT.CURRENCY, get.open.fun=DB.O, timeframe.tickvalue='M1',
                          get.ohlc.fun=DB.OHLC, timeframe.report='H1', parallel=PARALLEL.THRESHOLD.DB.SYMBOLS,
                          margin.base=1500, symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING) {
  within(report.phase2, {
    TICKETS.EDITED <- tickets.edited(TICKETS.EDITING, CURRENCY, get.open.fun, timeframe.tickvalue, symbols.setting, mysql.setting)
    TICKETS.MONEY <- tickets.money(TICKETS.SUPPORTED, set.init.money, include.middle, default.money)# %T>% print
    PERIOD <- tickets.period(TICKETS.EDITED)# %T>% print
    PRICE <- price.data(TICKETS.EDITED, PERIOD, get.ohlc.fun, timeframe.report, mysql.setting, parallel)# %T>% print
    TIMESERIE.TICKETS <- timeseries.tickets(TICKETS.EDITED, PRICE %>% price.data.with.tickvalue(
      get.open.fun, timeframe.tickvalue, currency, mysql.setting, symbols.setting), symbols.setting)# %T>% print
    TIMESERIE.SYMBOLS <- timeseries.symbols(TIMESERIE.TICKETS, TICKETS.MONEY)# %T>% print
    .timeserie.account <- timeseries.account(TIMESERIE.SYMBOLS, TICKETS.MONEY, margin.base)# %T>% print
    TIMESERIE.ACCOUNT <- .timeserie.account$timeseries.symbols# %T>% print
    SYMBOLS.PROFIT_VOLUME <- .timeserie.account$symbols.profit_volume
    SYMBOLS.RETURN <- .timeserie.account$symbols.return# %T>% print
    STATISTIC.ACCOUNT.PL <- tickets.statistics.pl.table(TICKETS.EDITED)# %T>% print
    STATISTIC.ACCOUNT.OTHERS <- tickets.statistics.others.table(TICKETS.EDITED, TIMESERIE.ACCOUNT, PERIOD$TRADE.DAYS, attr(PRICE, 'interval'))# %T>% print
    if (length(unique(TICKETS.EDITED[, SYMBOL])) > 1) {
      trade.days <- PERIOD$TRADE.DAYS
      STATISTIC.SYMBOLS.PL <-
        TICKETS.EDITED %>%
        setkey(SYMBOL, PL) %>%
        extract(
          j = tickets.statistics.pl.table(copy(.SD)),
          by = SYMBOL
        )# %T>% print
      STATISTIC.SYMBOLS.OTHERS <-
        TICKETS.EDITED %>%
        setkey(SYMBOL, PL) %>%
        extract(
          j = {
            tickets.statistics.others.table(copy(.SD), TIMESERIE.SYMBOLS[[SYMBOL[1]]], trade.days, attr(PRICE, 'interval'))# %T>% print
          },
          by = SYMBOL
        )# %T>% print
    }
    .timeserie.account <- NULL
    PHASE <- 3
  })
}

statistics.and.timeseries <- function(tickets.editing, currency=DEFAULT.CURRENCY, get.open.fun=DB.O, timeframe.tickvalue='M1',
                                      get.ohlc.fun=DB.OHLC, timeframe.report='H1', parallel=PARALLEL.THRESHOLD.DB.SYMBOLS,
                                      symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING) {
  within(list(), {
    TICKETS.EDITED <- tickets.edited(tickets.editing, currency, get.open.fun, mysql.setting, symbols.setting, mysql.setting)
    PERIOD <- tickets.period(TICKETS.EDITED)
    PRICE <- price.data(TICKETS.EDITED, PERIOD, get.ohlc.fun, timeframe.report, parallel)
    TIMESERIE.TICKETS <- timeseries.tickets(tickets.edited, PRICE %>% price.data.with.tickvalue(
      get.open.fun, timeframe.tickvalue, currency, symbols.setting
    ), symbols.setting)
    TIMESERIE.SYMBOLS <- timeseries.symbols(TIMESERIE.TICKETS)
    TIMESERIE.ACCOUNT <- timeseries.account(TIMESERIE.SYMBOLS)
  })
}
  
tickets.edited <- function(tickets.editing, currency=DEFAULT.CURRENCY, get.open.fun=DB.O, timeframe='M1',
                           symbols.setting=SYMBOLS.SETTING, mysql.setting) {
  tickets.editing %>%
    copy %>%
    setkey(SYMBOL) %>%
    extract(j = c('PIP', 'PROFIT', 'NPROFIT') := {
      symbol <- SYMBOL[1]
      pip <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
      tickvalue <- cal.tick.value(symbol, CTIME, get.open.fun, mysql.setting, timeframe, currency, symbols.setting)
      profit <- cal.profit(VOLUME, tickvalue, pip)
      list(round(pip, 0), profit, COMMISSION + TAXES + SWAP + PROFIT)
    }, by = SYMBOL) %>%
    extract(j = c('LOT.PROFIT', 'PL') :=
              list(PROFIT / VOLUME, ifelse(NPROFIT >= 0, 'PROFIT', 'LOSS')))
} # FINISH

tickets.period <- function(tickets.edited) {
  period.from <- min(tickets.edited[, OTIME], na.rm = TRUE)
  period.to <- max(tickets.edited[, CTIME], na.rm = TRUE)
  period.vector <-
    time.numeric.to.posixct(c(period.from, period.to)) %>%
    as.Date
  trade.days <-
    seq.Date(from = period.vector[1], to = period.vector[2], by = 'day') %>%
    wday %>%
    extract(!. %in% c(1,7)) %>%
    length
  nature.interval <- interval(period.vector[1], period.vector[2], tz = 'GMT')
  nature.period <- as.period(nature.interval)
  data.table(
    FROM = period.from,
    TO = period.to,
    NATURE.INTERVAL = nature.interval,
    NATURE.PERIOD = nature.period,
    TRADE.DAYS = trade.days
  )
} # FINISH

#### STATISTICS ####
# tickets.statistics <- function(tickets.edited) {
#   within(list(), {
#     STATISTIC.PL <- tickets.statistics.by.pl(tickets.edited)
#     STATISTIC.EXIT <- tickets.statistics.by.exit(tickets.edited)
#     STATISTIC.TYPE <- tickets.statistics.by.type(tickets.edited)
#     STATISTIC.SUMMARY <- tickets.statistics.by.pl(STATISTIC.PL)
#     STATISTIC.CONTINUOUS <- tickets.statistics.continuous(tickets.edited)
#   })
# }

tickets.statistics.pl.table <- function(tickets.edited) {
  tickets.statistics.by.pl(tickets.edited) %>%
    setkey(PL) %>%
    # extract(tickets.statistics.continuous(tickets.edited))
    extract(tickets.statistics.continuous(tickets.edited)) %>%
    rbind(.[, .(PL = 'TOTAL',
                N = sum(N),
                SUM = round(sum(SUM), 2),
                MEAN = round(sum(SUM) / sum(N), 2),
                PIP.SUM = round(sum(PIP.SUM), 2),
                PIP.MEAN = round(sum(PIP.SUM) / sum(N), 2),
                VOL.SUM = round(sum(VOL.SUM), 2),
                VOL.MEAN = round(sum(VOL.SUM) / sum(N), 2))],
          use.names = TRUE, fill = TRUE)
}

tickets.statistics.by.pl <- function(tickets.edited) {
  tickets.edited %>%
    setkey(PL) %>%
    extract(
      j = .(N = .N,
            SUM = sum(NPROFIT),
            MEAN = round(mean(NPROFIT), 2),
            MAX = round(ifelse(sign(NPROFIT[1]) == -1, min(NPROFIT), max(NPROFIT)), 2),
            PIP.SUM = sum(PIP),
            PIP.MEAN = round(mean(PIP), 2),
            PIP.MAX = ifelse(sign(PIP[1]) == -1, min(PIP), max(PIP)),
            VOL.SUM = sum(VOLUME),
            VOL.MEAN = round(mean(VOLUME), 2),
            VOL.MAX = round(max(VOLUME), 2),
            VOL.MIN = round(min(VOLUME), 2)),
      by = PL) %>%
    extract(c('PROFIT', 'LOSS')) %>%
    extract(is.na(N), 2:ncol(.) := list(0))
} # FINISH

tickets.statistics.others.table <- function(tickets.edited, timeseries, trade.day, interval) {
  mdd.mdd <- maxdrawdown(timeseries[, BALANCE.DELTA])
  returns <- timeseries[, RETURN]
  returns[is.na(returns)] <- 0
  return.serie <- cumprod(returns + 1)
  mddp.mdd <- maxdrawdown(return.serie)
  tickets.statistics.profit_yield_trade(tickets.edited, tail(cum.return(timeseries[, RETURN]), 1), trade.day) %>%
    cbind(., tickets.statistics.by.exit(tickets.edited),
          SUMMARY = c('SHARPE', 'PROF.FACTOR', 'LOT.PROF'),
          S.VALUE = c(round(sharpe.ratio(timeseries[, RETURN]), 2),
                    round(tickets.edited[PL == 'PROFIT', sum(NPROFIT), nomatch = 0] / -tickets.edited[PL == 'LOSS', sum(NPROFIT), nomatch = 0], 2),
                    round(tickets.edited[, sum(NPROFIT) / sum(VOLUME)], 2)),
          MDD = c('MDD', 'MDDP', NA_character_),
          MDD.VALUE = c(mdd.mdd$MDD %>% round(2),
                        (1 - return.serie[mddp.mdd$TO[1]] / return.serie[mddp.mdd$FROM[1]]) %>% multiply_by(100) %>% round(2),
                        NA_real_),
          MDD.PERIOD = c((mdd.mdd$TO[1] - mdd.mdd$FROM[1]) %>% multiply_by(interval),
                         (mddp.mdd$TO[1] - mddp.mdd$FROM[1]) %>% multiply_by(interval),
                         NA_real_)
    )
}

tickets.statistics.by.exit <- function(tickets.edited) {
  n.trade <- nrow(tickets.edited)
  tickets.edited %>%
    setkey(EXIT) %>%
    extract(
      i = !is.na(EXIT),
      j = .(N = .N,
            PERCENT = round(.N / n.trade * 100, 2),
            NPROFIT = round(sum(NPROFIT), 2)),
      by = EXIT) %>%
    extract(c('SL', 'TP', 'SO')) %>%
    extract(is.na(N), 2:ncol(.) := list(0))
} # FINISH

tickets.statistics.profit_yield_trade <- function(tickets.edited, yield, trade.day) {
  data.table(
    ITEM = c('PROFIT', 'YIELD', 'TRADE'),
    TOTAL = c(round(sum(tickets.edited[, NPROFIT]), 2), yield, nrow(tickets.edited))
  ) %>%
    extract(
      j = c('DAILY', 'WEEKLY', 'MONTHLY', 'YEARLY') := {
        daily <- round(TOTAL / trade.day, 2)
        list(daily, round(daily * 5, 2), round(daily * 21.76, 2), round(daily * 252, 2))
      }
    )
}

tickets.statistics.by.type <- function(tickets.edited) {
  tickets.edited %>%
    setkey(TYPE) %>%
    extract(
      j = .(N = .N,
            NVOLUME = sum(VOLUME)
      ),
      by = TYPE) %>%
    extract(c('BUY', 'SELL')) %>%
    extract(is.na(N), 2:ncol(.) := list(0))
} # FINISH

tickets.statistics.summary <- function(statistics.pl) {
  statistics.pl[j = .(
                      'PROFIT.FACTOR' = round(SUM[1] / -SUM[2], 2),
                      'PROFIT/LOT' = round(sum(SUM) / sum(VOL.SUM), 2)
                     
  )]
} # FINISH

tickets.statistics.continuous <- function(tickets.edited) {
  nprofit <-
    tickets.edited %>%
    setkey(CTIME) %>%
    extract(j = NPROFIT)
  col.name <- c('CON.N.MAX', 'CON.N.MEAN', 'CON.MAX', 'CON.MEAN')
  row.name <- c('PROFIT', 'LOSS')
  con.table <-
    data.table(PL = row.name, matrix(data = 0, nrow = length(row.name), ncol = length(col.name),
                                         dimnames = list(NULL, col.name))) %>%
    setkey(PL)
  continuous <- cal.continuous(nprofit)
  if (length(continuous$UP.FROM)) {
    up.n <- with(continuous, UP.TO - UP.FROM) + 1
    up.pl <- mapply(function(from, to) {
      sum(nprofit[from:to])
    }, from = continuous$UP.FROM, to = continuous$UP.TO)
    con.table['PROFIT', (col.name) := list(max(up.n), round(mean(up.n), 2), round(max(up.pl), 2), round(mean(up.pl), 2))]
  }
  if (length(continuous$DN.FROM)) {
    dn.n <- with(continuous, DN.TO - DN.FROM) + 1
    dn.pl <- mapply(function(from, to) {
      sum(nprofit[from:to])
    }, from = continuous$DN.FROM, to = continuous$DN.TO)
    con.table['LOSS', (col.name) := list(max(dn.n), round(mean(dn.n), 2), round(max(dn.pl), 2), round(mean(dn.pl), 2))]
  }
  con.table[row.name]
} # FINISH

#### PRICE DATA ####
price.data <- function(tickets.edited, tickets.period, get.ohlc.fun=DB.OHLC, timeframe='H1',
                       mysql.setting=MYSQL.SETTING, parallel=PARALLEL.THRESHOLD.DB.SYMBOLS) {
  symbols <-
    tickets.edited[, SYMBOL] %>%
    unique
  time.period <- tickets.period$NATURE.INTERVAL
  get.ohlc.fun(symbols, int_start(time.period), int_end(time.period), timeframe, mysql.setting, parallel) %>%
    setattr('interval', TIMEFRAME.INTERVAL[timeframe])
}

price.data.with.tickvalue <- function(price.data,
                                      get.open.fun=DB.O, tickvalue.timeframe='M1', currency=DEFAULT.CURRENCY,
                                      mysql.setting=MYSQL.SETTING, symbols.setting=SYMBOLS.SETTING) {
  interval <- attr(price.data, 'interval')
  price.data %<>% copy
  serie.columns <- c('PROFIT', 'FLOATING', 'PL.VOLUME', 'VOLUME', 'MAX.FLOATING', 'MIN.FLOATING')
  symbols <- names(price.data)
  lapply(symbols, function(symbol) {
    price.data[[symbol]] %>%
      extract(j = TICKVALUE := cal.tick.value(symbol, TIME, get.open.fun, mysql.setting, tickvalue.timeframe,
                                               currency, symbols.setting)) %>%
      extract(j = (serie.columns) := 0)
  }) %>%
    set_names(symbols) %>%
    setattr('interval', interval)
}

#### TIMESERIES ####
timeseries <- function(tickets.edited, price.data.for.timeseries, symbols.setting=SYMBOLS.SETTING) {
  tickets.serie <- timeseries.tickets(tickets.edited, price.data.for.timeseries, symbols.setting)
  symbols.serie <- timeseries.symbols(tickets.serie)
  account.serie <- timeseries.account(symbols.serie)
  list(
    TICKETS = tickets.serie,
    SYMBOLS = symbols.serie,
    ACCOUNT = account.serie
  )
} # FINISH

timeseries.tickets <- function(tickets.edited, price.data, symbols.setting=SYMBOLS.SETTING) {
  fun.env <- environment()
  env.tickets.series <- list()
  extra.columns <-
    tickets.edited %>%
    setkey(SYMBOL) %>%
    extract(
      j = {
        symbol <- SYMBOL[1]
        ticket.timeserie <-
          mapply(timeseries.one.ticket, OTIME, CTIME, NPROFIT, TYPE, VOLUME, OPRICE,
                 MoreArgs = list(symbols.setting[symbol, DIGITS], symbols.setting[symbol, SPREAD],
                                 price.data[[symbol]]),
                 SIMPLIFY = FALSE) %>%
          set_names(paste('T', TICKET, sep = '_'))
        ticket.timeserie %>%
          list %>%
          append(env.tickets.series, .) %>%
          assign('env.tickets.series', ., envir = fun.env)
        mapply(tickets.extra.columns, ticket.timeserie, TICKET, OTIME, CTIME,
               MoreArgs = list(attr(price.data, 'interval')),
               SIMPLIFY = FALSE) %>%
          do.call(rbind, .)
      },
      by = SYMBOL
    ) %>%
    setkey(TICKET)
  tickets.edited %>%
    setkey(TICKET) %>%
    extract(
      j = c('PERIOD', 'MFP', 'MFPP', 'MFL', 'MFLP', 'OTIME', 'CTIME') := 
        with(extra.columns, list(PERIOD, MFP, MFPP, MFL, MFLP, time.numeric.to.posixct(OTIME), time.numeric.to.posixct(CTIME)))
    )
  setNames(env.tickets.series, tickets.edited[, SYMBOL] %>% table %>% names)
} # FINISH

timeseries.symbols <- function(timeserie.tickets, money.tickets) {
  fun.env <- environment()
  env.symbols.series <- list()
  lapply(timeserie.tickets, function(symbol.list) {
    symbol.table <- 0
    symbol.timeserie <-
      lapply(symbol.list, function(ticket.serie) {
        symbol.table <<- symbol.table +
          ticket.serie[, .(BALANCE.DELTA = PROFIT + FLOATING, NET.VOLUME = PL.VOLUME, SUM.VOLUME = VOLUME)]
      })
    symbol.table %>%
      extract(j = c('TIME', 'MONEY', 'EQUITY', 'RETURN') := {
        serie.time <- symbol.list[[1]][, TIME]
        money.delta <- money.delta(money.tickets, serie.time)
        equity <- BALANCE.DELTA + cumsum(money.delta)
        returns <- c(0, diff(BALANCE.DELTA) / equity[-length(equity)])
        infinite.index <- which(is.infinite(returns))
        returns[infinite.index] <- BALANCE.DELTA[infinite.index] / as.numeric(money.delta[infinite.index])
        list(serie.time, money.delta, equity, returns)
      }) %>%
      extract(
        i = EQUITY != 0
      ) %>%
      setkey(TIME) %>%
      list %>%
      append(env.symbols.series, .) %>%
      assign('env.symbols.series', ., envir = fun.env)
  })
  setNames(env.symbols.series, names(timeserie.tickets))
} # FINISH

timeseries.account <- function(timeseries.symbols, money.tickets, margin.base=1500) {
  fun.env <- environment()
  symbols.profit_volume <- list()
  symbols.return.serie <- list()
  symbol.names <- names(timeseries.symbols)
  account.table <- 0
  len <- length(timeseries.symbols)
  intersection.time <-
    lapply(timeseries.symbols, function(ts) {
      ts[, TIME]
    }) %>%
    do.call(c, .) %>%
    table %>%
    extract(. == len) %>%
    names %>%
    as.numeric
  mapply(function(symbol.serie, symbol) {
    fun.env$account.table %<>% add(symbol.serie[.(intersection.time),
                                                .(BALANCE.DELTA = BALANCE.DELTA, NET.VOLUME = abs(NET.VOLUME), SUM.VOLUME = SUM.VOLUME)])
    fun.env$symbols.profit_volume %<>% append(list(symbol.serie[.(intersection.time),
                                                                .(TIME = TIME, SYMBOL = symbol, BALANCE.DELTA = BALANCE.DELTA, NET.VOLUME = NET.VOLUME)]))
    fun.env$symbols.return.serie %<>% append(list(symbol.serie[.(intersection.time), RETURN]))
  }, timeseries.symbols, symbol.names)
  symbols.profit_volume %<>%
    do.call(rbind, .) %>%
    rbind(account.table[, .(TIME = intersection.time, SYMBOL = 'PORTFOLIO', BALANCE.DELTA, NET.VOLUME)])%>%
    extract(
      j = TIME := time.numeric.to.posixct(TIME)
    ) 
  symbols.return.serie %<>%
    do.call(cbind, .) %>%
    as.data.table %>%
    setnames(symbol.names) %>%
    cbind(TIME = time.numeric.to.posixct(intersection.time), .)
  account.table %>%
    extract(j = c('TIME', 'MONEY', 'EQUITY', 'RETURN', 'MARGIN.USED', 'MARGIN.FREE') := {
      money.delta <- money.delta(money.tickets, intersection.time)
      equity <- BALANCE.DELTA + cumsum(money.delta)
      returns <- c(0, diff(BALANCE.DELTA) / equity[-length(equity)])
      infinite.index <- which(is.infinite(returns))
      returns[infinite.index] <- BALANCE.DELTA[infinite.index] / as.numeric(money.delta[infinite.index])
      margin.used <- NET.VOLUME * margin.base
      list(intersection.time, money.delta, equity, returns, margin.used, equity - margin.used)
    }) %>%
    extract(
      j = TIME := time.numeric.to.posixct(TIME)
    ) %>%
    setkey(TIME) %>%
    list(
      timeseries.symbols = .,
      symbols.profit_volume = symbols.profit_volume,
      symbols.return = symbols.return.serie
    )
} # FINISH

timeseries.one.ticket <- function(otime, ctime, nprofit, type, volume,
                                  oprice, digit, spread, symbol.price.data, interval) {
  copy(symbol.price.data) %>%
  setkey(TIME) %>%
  extract(TIME >= ctime, PROFIT := nprofit) %>%
  extract(
    i = TIME >= otime & TIME < ctime,
    j = c('FLOATING', 'PL.VOLUME', 'VOLUME', 'MAX.FLOATING', 'MIN.FLOATING') := {
      digit.factor <- 10 ^ digit
      if (type == 'BUY') {
        pl.volume <- volume
        floating.pip <- (OPEN - oprice) * digit.factor
        max.floating.pip <- (HIGH - oprice) * digit.factor
        min.floating.pip <- (LOW - oprice) * digit.factor
      } else {
        pl.volume <- -volume
        floating.pip <- (oprice - OPEN) * digit.factor - spread
        max.floating.pip <- (oprice - LOW) * digit.factor - spread
        min.floating.pip <- (oprice - HIGH) * digit.factor - spread
      }
      list(cal.profit(volume, TICKVALUE, floating.pip), pl.volume, volume,
           cal.profit(volume, TICKVALUE, max.floating.pip),
           cal.profit(volume, TICKVALUE, min.floating.pip))
    }
  ) %>%
  extract(
    j = c('OPEN', 'HIGH', 'LOW', 'CLOSE', 'TICKVALUE') := NULL
  ) %>%
  setkey(TIME)
} # FINISH

tickets.extra.columns <- function(timeseries.one.ticket, ticket, otime, ctime, interval) {
  floating.part <- timeseries.one.ticket[VOLUME != 0, nomatch = 0]
  otime.shift <- interval - otime %% interval
  ctime.shift <- ctime %% interval
  if (rows <- nrow(floating.part)) {
    period <- otime.shift + ctime.shift + rows * interval
    mfp <- floating.part[, max(MAX.FLOATING)]
    mfl <- floating.part[, min(MIN.FLOATING)] %>% ifelse(. > 0, 0, .)
    mfpp <- otime.shift + (floating.part[1:which.max(MAX.FLOATING), .N] - 0.5) * interval
    mflp <- otime.shift + (floating.part[1:which.min(MIN.FLOATING), .N] - 0.5) * interval
  } else {
    period <- min(otime.shift + ctime.shift, ctime - otime)
    mfp <- NA_real_
    mfl <- NA_real_
    mfpp <- NA_real_
    mflp <- NA_real_
  }
  data.table(TICKET = ticket, PERIOD = period, MFP = mfp, MFPP = mfpp, MFL = mfl, MFLP = mflp)
}

#### MONEY ####
tickets.money <- function(tickets.supported, set.init.money=NULL, include.middle=TRUE, default.money=DEFAULT.INIT.MONEY) {
  first.trade.time <-
    tickets.supported %>%
    setkey(GROUP) %>%
    extract(!'MONEY', min(OTIME))
  
  if (!is.null(set.init.money)) {
    init.part <-
      data.table(
        TICKET = 0,
        OTIME = first.trade.time - 1,
        PROFIT = set.init.money
      ) %>%
      build.tickets(., 'MONEY')[[1]]
  } else {
    tickets.money.raw <-
      tickets.supported['MONEY'] %>%
      extract(OTIME < first.trade.time, nomatch = 0)
    if (nrow(tickets.money.raw)) {
      init.part <- tickets.money.raw
    } else {
      init.part <-
        data.table(
          TICKET = 0,
          OTIME = first.trade.time - 1,
          PROFIT = default.money
        ) %>%
        build.tickets(., 'MONEY') %>%
        extract2(1)
    }
  }
  # init.part <-
  #   (if (!is.null(set.init.money)) {
  #     data.table(
  #       TICKET = 0,
  #       OTIME = first.trade.time - 1,
  #       PROFIT = set.init.money
  #     ) %>%
  #       build.tickets('MONEY')
  #   } else {
  #     tickets.money.raw <-
  #       tickets.supported['MONEY'] %>%
  #       extract(OTIME < first.trade.time, nomatch = 0)
  #     if (nrow(tickets.money.raw)) {
  #       tickets.money.raw
  #     } else {
  #       data.table(
  #         TICKET = 0,
  #         OTIME = first.trade.time - 1,
  #         PROFIT = default.money
  #       ) %>%
  #         build.tickets('MONEY')
  #     }
  #   })
  if (include.middle) {
    middle.part <- tickets.supported[GROUP == 'MONEY' & OTIME > first.trade.time, nomatch = 0]
    if (nrow(middle.part)) {
      return(rbind(init.part, middle.part, use.names=TRUE, fill = TRUE))
    }
  }
  init.part
}

money.delta <- function(tickets.money, time.vector) {
  serie <- vector('numeric', length(time.vector))
  mapply(function(time, value) {
    serie[which(time.vector > time)[1]] <<- value
  }, tickets.money[, 'OTIME'], tickets.money[, 'PROFIT'])
  serie
}

#### UTILS ####
cal.continuous <- function(x) {
  if (!(len <- length(x))) {
    return(NULL)
  }
  signs <- ifelse(x >= 0, 1, 0)
  turns <- diff(signs)
  dn.to <- which(turns == 1)
  up.from <- dn.to + 1
  up.to <- which(turns == -1)
  dn.from <- up.to + 1
  if (signs[1]) {
    up.from %<>% c(1, .)
  } else {
    dn.from %<>% c(1, .)
  }
  if (signs[len]) {
    up.to %<>% c(len)
  } else {
    dn.to %<>% c(len)
  }
  list(UP.FROM = up.from, UP.TO = up.to, DN.FROM = dn.from, DN.TO = dn.to)
} # FINISH

maxdrawdown <- function(x) {
  if (!(len <- length(x))) {
    return(NULL)
  }
  cum.drawdown <- cummax(x) - x
  mdd <- max(cum.drawdown)
  to <- which(mdd == cum.drawdown)
  cum.max <- which(cum.drawdown  == 0)
  from <- sapply(to, function(x) {
    cum.max[sum(cum.max <= x)]
  })
  list(MDD = mdd, FROM = from, TO = to)
} # FINISH

cum.return <- function(x, percent=T, digits=2) {
  x[is.na(x)] <- 0
  res <- cumprod(x + 1) - 1
  if (percent) {
    res %<>% multiply_by(100)
  }
  round(res, digits)
}

time.num.to.period.char <- function(time.num) {
  time.numeric.to.posixct(time.num) %>%
    as.character %>%
    substr(12, 20) %>% ifelse(time.num >= 86400, paste0(time.num %/% 86400, 'D ', .), .)
}
