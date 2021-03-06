library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####

#### @PATCH NOTE@ ####
## 2017-03-26: Version 0.1


reports.phase3 <- function(report.phase2, get.open.fun=DB.O, timeframe.tickvalue='M1',
                           symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING,
                           default.money=DEFAULT.INIT.MONEY, get.ohlc.fun=DB.OHLC, timeframe.report='H1',
                           parallel=PARALLEL.THRESHOLD.DB.SYMBOLS, margin.base=1500,
                           parallel.tickets=PARALLEL.THRESHOLD.GENERATE.TICKETS) {
  if (is.numeric(parallel.tickets)) {
    parallel.tickets <- length(report.phase2) >= parallel.tickets
  }
  if (!parallel.tickets) {
    phase3data <- lapply(report.phase2, report.phase3,
                         get.open.fun, timeframe.tickvalue, symbols.setting, mysql.setting, default.money,
                         get.ohlc.fun, timeframe.report, parallel, margin.base)
  } else {
    cluster <- makeCluster(detectCores() - 1)
    phase3data <- parLapply(cluster, report.phase2, report.phase3,
                            get.open.fun, timeframe.tickvalue, symbols.setting, mysql.setting, default.money,
                            get.ohlc.fun, timeframe.report, parallel, margin.base)
    stopCluster(cluster)
  }
  phase3data
}

report.phase3 <- function(report.phase2, get.open.fun=DB.O, timeframe.tickvalue='M1',
                          symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING,
                          default.money=DEFAULT.INIT.MONEY, get.ohlc.fun=DB.OHLC, timeframe.report='H1',
                          parallel=PARALLEL.THRESHOLD.DB.SYMBOLS, margin.base=1500) {
  # if (is.null(report.phase2$TICKETS.EDIT)) {
  #   report.phase2$TICKETS.EDIT <- tickets.edit.init(report.phase2)
  # }
  within(report.phase2, {
    tickets.edited(TICKETS.EDIT, CURRENCY, get.open.fun, timeframe.tickvalue, symbols.setting, mysql.setting)
    TICKETS.MONEY <- tickets.money(TICKETS.EDIT, TICKETS[GROUP == 'MONEY'], TICKETS.GROUP.EDIT$MONEY.INIT,
                                   TICKETS.GROUP.EDIT$MONEY.MIDDLE.INCLUDE, default.money)
    PERIOD <- tickets.period(TICKETS.EDIT, TICKETS.MONEY)
    SYMBOLS <- TICKETS.EDIT[, SYMBOL] %>% unique
    PRICE <- price.data(SYMBOLS, PERIOD$NATURE.INTERVAL %>% int_start, PERIOD$NATURE.INTERVAL %>% int_end,
                        get.ohlc.fun, timeframe.report, mysql.setting, parallel)
    TIMESERIE.TICKETS <- timeseries.tickets(TICKETS.EDIT, PRICE %>% copy %>% price.data.with.tickvalue(
      get.open.fun, timeframe.tickvalue, CURRENCY, mysql.setting, symbols.setting), symbols.setting)
    TIMESERIE.SYMBOLS <- timeseries.symbols(TIMESERIE.TICKETS, TICKETS.MONEY)
    .timeserie.account <- timeseries.account(TIMESERIE.SYMBOLS, TICKETS.MONEY, margin.base)
    TIMESERIE.ACCOUNT <- .timeserie.account$timeseries.symbols
    SYMBOLS.PROFIT_VOLUME <- .timeserie.account$symbols.profit_volume
    SYMBOLS.RETURN <- .timeserie.account$symbols.return
    STATISTIC.ACCOUNT.PL <- tickets.statistics.pl.table(TICKETS.EDIT)
    STATISTIC.ACCOUNT.OTHERS <-
      tickets.statistics.others.table(TICKETS.EDIT, TIMESERIE.ACCOUNT, PERIOD$TRADE.DAYS, attr(PRICE, 'interval'))
    if (length(SYMBOLS) > 1) {
      trade.days <- PERIOD$TRADE.DAYS
      STATISTIC.SYMBOLS.PL <-
        TICKETS.EDIT %>%
        setkey(SYMBOL, PL) %>%
        extract(
          j = tickets.statistics.pl.table(.SD %>% copy),
          by = SYMBOL
        )
      STATISTIC.SYMBOLS.OTHERS <-
        TICKETS.EDIT %>%
        setkey(SYMBOL, PL) %>%
        extract(
          j = {
            tickets.statistics.others.table(.SD %>% copy, TIMESERIE.SYMBOLS[[SYMBOL[1]]],
                                            trade.days, attr(PRICE, 'interval'))
          },
          by = SYMBOL
        )
    }
    .timeserie.account <- NULL
    PHASE <- 3
  })
}

tickets.edited <- function(tickets.edit, currency, get.open.fun=DB.O, timeframe.tickvalue='M1',
                           symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING) {
  tickets.edit %>% setkey(SYMBOL) %>%
    extract(
      i = is.na(PROFIT),
      j = PROFIT := {
        symbol <- SYMBOL[1]
        pip <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
        tickvalue <- cal.tick.value(symbol, CTIME, get.open.fun, mysql.setting, timeframe.tickvalue,
                                    currency, symbols.setting)
        cal.profit(VOLUME, tickvalue, pip)},
      by = SYMBOL) %>%
    extract(
      j = c('PIP', 'NPROFIT') := {
        symbol <- SYMBOL[1]
        pip <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
        list(round(pip, 0), COMMISSION + TAXES + SWAP + PROFIT)
      },
      by = SYMBOL) %>%
    extract(
      j = c('LOT.PROFIT', 'PL') := list(PROFIT / VOLUME, ifelse(NPROFIT >= 0, 'PROFIT', 'LOSS'))
    )
}


#### TICKETS MONEY ####
tickets.money <- function(tickets.edited, money, money.init, money.middle, default.money=DEFAULT.INIT.MONEY) {
  first.trade.time <- tickets.edited %>% extract(GROUP != 'MONEY', min(OTIME))
  if (!is.null(money.init)) {
    data.table(
      TICKET = 0,
      OTIME = first.trade.time - 1,
      PROFIT = money.init
    ) %>% build.tickets('MONEY')
  } else {
    tickets.init.money <- money[OTIME <= first.trade.time, nomatch = 0]
    if (nrow(tickets.init.money)) {
      build.tickets(tickets.init.money, 'MONEY')
    } else {
      data.table(
        TICKET = 0,
        OTIME = first.trade.time - 1,
        PROFIT = default.money
      ) %>% build.tickets('MONEY')
    }
  }
  if (money.middle) {
    money[OTIME > first.trade.time, nomatch = 0] %>% build.tickets('MONEY')
  }
  build.tickets.raw()
}

#### TICKETS PERIOD ####
tickets.period <- function(tickets.edited, tickets.money) {
  period.from <- min(tickets.money[, OTIME], na.rm = TRUE)
  period.to <- max(tickets.edited[, CTIME], na.rm = TRUE)
  period.vector <-
    c(period.from, period.to) %>%
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

#### PRICE DATA ####
price.data <- function(symbols, from, to, get.ohlc.fun=DB.OHLC, timeframe.report='H1',
                       mysql.setting=MYSQL.SETTING, parallel=PARALLEL.THRESHOLD.DB.SYMBOLS) {
  get.ohlc.fun(symbols, from, to, timeframe.report, mysql.setting, parallel) %>%
    setattr('interval', TIMEFRAME.INTERVAL[timeframe.report])
}

price.data.with.tickvalue <- function(price.data,
                                      get.open.fun=DB.O, tickvalue.timeframe='M1', currency=DEFAULT.CURRENCY,
                                      mysql.setting=MYSQL.SETTING, symbols.setting=SYMBOLS.SETTING) {
  interval <- attr(price.data, 'interval')
  serie.columns <- c('PROFIT', 'FLOATING', 'PL.VOLUME', 'VOLUME', 'MAX.FLOATING', 'MIN.FLOATING')
  symbols <- names(price.data)
  lapply(symbols, function(symbol) {
    price.data[[symbol]] %>% copy %>%
      extract(j = TICKVALUE := cal.tick.value(symbol, TIME, get.open.fun, mysql.setting, tickvalue.timeframe,
                                              currency, symbols.setting)) %>%
      extract(j = (serie.columns) := 0)
  }) %>%
    set_names(symbols) %>%
    setattr('interval', interval)
}

#### TIMESERIES ####
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
        mapply(tickets.extra.columns, ticket.timeserie, TICKET, OTIME %>% as.numeric, CTIME %>% as.numeric,
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
      j = c('PERIOD', 'MFP', 'MFPP', 'MFL', 'MFLP') := 
        with(extra.columns, list(PERIOD, MFP, MFPP, MFL, MFLP))
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
    symbol.table %>% copy %>%
      extract(
        j = c('TIME', 'MONEY', 'EQUITY', 'RETURN') := {
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
    if (len == 0) {
      timeseries.symbols[[1]][, TIME]
    } else {
      lapply(timeseries.symbols, function(ts) {
        ts[, TIME] %>% as.character
      }) %>%
        do.call(c, .) %>%
        table %>%
        extract(. == len) %>%
        names %>%
        ymd_hms(tz = 'GMT')
    }
  mapply(function(symbol.serie, symbol) {
    fun.env$account.table %<>%
      add(symbol.serie[.(intersection.time),
                       .(BALANCE.DELTA = BALANCE.DELTA, NET.VOLUME = abs(NET.VOLUME), SUM.VOLUME = SUM.VOLUME)])
    fun.env$symbols.profit_volume %<>%
      append(list(symbol.serie[.(intersection.time),
                               .(TIME = TIME, SYMBOL = symbol, BALANCE.DELTA = BALANCE.DELTA,
                                 NET.VOLUME = NET.VOLUME)]))
    fun.env$symbols.return.serie %<>% append(list(symbol.serie[.(intersection.time), RETURN]))
  }, timeseries.symbols, symbol.names)
  symbols.profit_volume %<>%
    do.call(rbind, .) %>%
    rbind(account.table[, .(TIME = intersection.time, SYMBOL = 'PORTFOLIO', BALANCE.DELTA, NET.VOLUME)])
  symbols.return.serie %<>%
    do.call(cbind, .) %>%
    as.data.table %>%
    setnames(symbol.names) %>%
    cbind(TIME = intersection.time, .)
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
    mfpp <- (otime.shift + (floating.part[1:which.max(MAX.FLOATING), .N] - 0.5) * interval) %>% time.num.to.period.char
    mflp <- (otime.shift + (floating.part[1:which.min(MIN.FLOATING), .N] - 0.5) * interval) %>% time.num.to.period.char
  } else {
    period <- min(otime.shift + ctime.shift, ctime - otime)
    mfp <- NA_real_
    mfl <- NA_real_
    mfpp <- NA_character_
    mflp <- NA_character_
  }
  data.table(TICKET = ticket, PERIOD = period, MFP = mfp, MFPP = mfpp, MFL = mfl, MFLP = mflp)
}

#### STATISTICS ####
tickets.statistics.pl.table <- function(tickets.edited) {
  tickets.statistics.by.pl(tickets.edited) %>%
    extract(tickets.statistics.continuous(tickets.edited), on = 'PL') %>%
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
                      round(tickets.edited[PL == 'PROFIT', sum(NPROFIT), nomatch = 0] /
                              -tickets.edited[PL == 'LOSS', sum(NPROFIT), nomatch = 0], 2),
                      round(tickets.edited[, sum(NPROFIT) / sum(VOLUME)], 2)),
          MDD = c('MDD', 'MDDP', NA_character_),
          MDD.VALUE = c(mdd.mdd$MDD %>% round(2),
                        (1 - return.serie[mddp.mdd$TO[1]] / return.serie[mddp.mdd$FROM[1]]) %>% multiply_by(100) %>%
                          round(2), NA_real_),
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
      i = c(1, 3),
      j = c('DAILY', 'WEEKLY', 'MONTHLY', 'YEARLY') := {
        daily <- round(TOTAL / trade.day, 2)
        list(daily, round(daily * 5, 2), round(daily * 21.76, 2), round(daily * 252, 2))
      }
    ) %>%
    extract(
      i = c(2),
      j = c('DAILY', 'WEEKLY', 'MONTHLY', 'YEARLY') := {
        daily_add1 <- (TOTAL + 1) ^ (1 / trade.day)
        c(daily_add1, daily_add1 ^ 5, daily_add1 ^ 21.76, daily_add1 ^ 252) %>% subtract(1) %>% round(2) %>%
          as.list
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
    con.table['PROFIT', (col.name) := list(max(up.n), round(mean(up.n), 2),
                                           round(max(up.pl), 2), round(mean(up.pl), 2))]
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
  as.POSIXct(time.num, origin = '1970-01-01', tz = 'GMT') %>%
    as.character %>%
    substr(12, 20) %>% ifelse(time.num >= 86400, paste0(time.num %/% 86400, 'D ', .), .)
}

money.delta <- function(tickets.money, time.vector) {
  serie <- vector('numeric', length(time.vector))
  mapply(function(time, value) {
    serie[which(time.vector > time)[1]] <<- value
  }, tickets.money[, OTIME], tickets.money[, PROFIT])
  serie
}

sharpe.ratio <- function(x) {
  # ''' calculate sharpe ratio '''
  # 2016-08-19: Done
  mean(x) / sd(x)
}