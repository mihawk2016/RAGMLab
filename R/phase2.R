library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####
## 2017-02-15: mt4 ea method can be more (data.table)able

#### @PATCH NOTE@ ####
## 2017-02-12: Version 0.1










html.report.phase2 <- function(report.phase1, default.currency=DEFAULT.CURRENCY, default.leverage=DEFAULT.LEVERAGE,
                               get.open.fun=DB.O, mysql.setting=MYSQL.SETTING, timeframe.tickvalue='M1',
                               symbols.setting=SYMBOLS.SETTING) {
  within(report.phase1, {
    CURRENCY <- report.currency(INFOS, default.currency)
    LEVERAGE <- report.leverage(INFOS, default.leverage)
    TICKETS <- html.tickets.raw(report.phase1, CURRENCY, get.open.fun,
                                mysql.setting, timeframe.tickvalue, symbols.setting)
    ITEM.SYMBOL.MAPPING <- item.symbol.mapping(TICKETS, symbols.setting[, SYMBOL])
    SUPPORTED.ITEM <- supported.items(ITEM.SYMBOL.MAPPING)
    UNSUPPORTED.ITEM <- unsupported.items(ITEM.SYMBOL.MAPPING)
    TICKETS[, SYMBOL := ITEM.SYMBOL.MAPPING[ITEM[1]], by = ITEM]
    PHASE <- 2
  })
}


#### TICKETS TYPE ####
tickets.supported <- function(report) {
  report$TICKETS %>% extract(i = GROUP == 'MONEY' | !is.na(SYMBOL))
} # FINISH

#### FETCH TICKETS RAW ####
html.tickets.raw <- function(report.phase1, currency, get.open.fun=DB.O, mysql.setting=MYSQL.SETTING,
                             timeframe.tickvalue='M1', symbols.setting=SYMBOLS.SETTING) {
  switch(
    report.phase1$INFOS[, TYPE],
    'MT4-EA' = fetch.html.data.tickets.mt4ea(report.phase1, get.open.fun, mysql.setting, timeframe.tickvalue,
                                             currency, symbols.setting),
    'MT4-Trade' = fetch.html.data.tickets.mt4trade(report.phase1),
    'MT5-EA' = fetch.html.data.tickets.mt5ea(report.phase1, get.open.fun, mysql.setting, timeframe.tickvalue,
                                             currency, symbols.setting),
    'MT5-Trade' = fetch.html.data.tickets.mt5trade(report.phase1, get.open.fun, mysql.setting, timeframe.tickvalue,
                                                   currency, symbols.setting),
    'MT4M-Closed' = fetch.html.data.tickets.mt4m_closed(report.phase1),
    'MT4M-Raw' = fetch.html.data.tickets.mt4m_raw(report.phase1)
    )
} # FINISH

fetch.html.data.tickets.mt4ea <- function(report.phase1, get.open.fun, mysql.setting, timeframe.tickvalue,
                                          currency, symbols.setting) {
  table <-
    readHTMLTable(report.phase1$PATH, stringsAsFactors = FALSE, encoding = 'GBK', which = 2,
                  colClasses = c('character', time.char.to.posixct, 'character', num.char.to.num, num.char.to.num,
                                 num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num, 'character')) %>%
    as.data.table %>%
    set_colnames(c('deal', 'time', 'type', 'ticket', 'volume', 'price', 'sl', 'tp', 'profit', 'balance')) %>%
    extract(type != 'modify', !c('deal', 'balance'), with = FALSE)
  data.table(
    TICKET = 0,
    OTIME = report.phase1$.DEPOSIT.TIME,
    PROFIT = report.phase1$.DEPOSIT
  ) %>% build.tickets('MONEY')
  rows <- nrow(table)
  if (!rows) {
    return(build.tickets.raw())
  }
  item <- report.phase1$.ITEM
  table.index <- 1:rows
  table.types <- table[, type]
  table.tickets <- table[, ticket]
  pending.close.part.index <- which(table.types == 'delete')
  if (length(pending.close.part.index)) {
    pending.tickets.ticket <- table.tickets[pending.close.part.index]
    table.index %<>% setdiff(pending.close.part.index)
    pending.open.part.index <- table.index[table.tickets[table.index] %in% pending.tickets.ticket]
    table.index %<>% setdiff(pending.open.part.index)
    merge(table[pending.open.part.index, c(1, 2, 3, 5), with = FALSE],
          table[pending.close.part.index, c(1, 3:8), with = FALSE],
          by = 'ticket') %>%
      set_colnames(c('TICKET', 'OTIME', 'TYPE', 'OPRICE', 'CTIME', 'VOLUME', 'CPRICE', 'SL', 'TP', 'PROFIT')) %>%
      extract(j = (c('ITEM', 'COMMENT', 'TYPE')) := list(item, 'canceled', toupper(TYPE))) %>%
      build.tickets('PENDING')
  }
  pending.of.closed.ticktets.index <-
    table.index[grepl('(buy|sell) (limit|stop)', table.types[table.index], ignore.case = TRUE)]
  if (length(pending.of.closed.ticktets.index) ) {
    table.index %<>% setdiff(pending.of.closed.ticktets.index)
  }
  if (length(table.index)) {
    closed.tickets.open.part.index <- table.index[grepl('(buy|sell)', table.types[table.index], ignore.case = TRUE)]
    closed.tickets.close.part.index <- table.index %<>% setdiff(closed.tickets.open.part.index)
    part.closed.index <- which(table[closed.tickets.open.part.index, volume] !=
                                 table[closed.tickets.close.part.index, volume])
    closed.tickets <- merge(table[closed.tickets.open.part.index, c(1:3, 5), with = FALSE],
                            table[closed.tickets.close.part.index],
                            by = 'ticket')
    if (length(part.closed.index)) {
      closed.tickets[part.closed.index + 1, time.x := NA]
      closed.tickets[, time.x := na.locf(time.x)]
    }
    closed.tickets %<>%
      set_colnames(c('TICKET', 'OTIME', 'TYPE', 'OPRICE',
                     'CTIME', 'COMMENT', 'VOLUME', 'CPRICE', 'SL', 'TP', 'PROFIT')) %>%
      extract(j = c('ITEM', 'TYPE') := list(item, toupper(TYPE))) %>%
      extract(
        i = CTIME >= report.phase1$.END.TIME - 60 & COMMENT == 'close at stop',
        j = EXIT := paste(COMMENT, 'so', sep = ' / '))
    symbol <- item.to.symbol(item, symbols.setting[, SYMBOL])
    if (!is.na(symbol)) {
      closed.tickets[, c('SWAP', 'PROFIT') := {
        pips <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
        tickvalue <- cal.tick.value(symbol, CTIME, get.open.fun, mysql.setting, timeframe.tickvalue,
                                    currency, symbols.setting)
        new.profit <- cal.profit(VOLUME, tickvalue, pips)
        list(round(PROFIT - new.profit, 2), new.profit)
      }]
    }
    build.tickets(closed.tickets, 'CLOSED')
  }
  build.tickets.raw()
} # FINISH

fetch.html.data.tickets.mt4trade <- function(report.phase1) {
  table <-
    suppressWarnings(
      readHTMLTable(report.phase1$PATH, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1,
                    colClasses = c('numeric', time.char.to.posixct, toupper, num.char.to.num, toupper,
                                   num.char.to.num, num.char.to.num, num.char.to.num, time.char.to.posixct,
                                   num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num)
      )
    ) %>% as.data.table
  ticket.index <-
    table[j = V1] %>%
    is.na %>%
    not %>%
    which
  if (!length(ticket.index)) {
    return(NULL)
  }
  table %<>%
    extract(ticket.index) %>%
    extract(j = COMMENT := report.phase1$.COMMENT[ticket.index]) %>%
    set_colnames(names(TICKETS.TEMPLATE)[1:15]) %>%
    setkey(CTIME) %>%
    extract(j = NAs := rowSums(is.na(.))) %>%
    setkey(NAs)
  table[.(10), nomatch = 0] %>%
    extract(j = PROFIT := num.char.to.num(ITEM)) %>%
    extract(j = .(TICKET, OTIME, PROFIT, COMMENT)) %>%
    build.tickets('MONEY')
  table[.(0), nomatch = 0] %>%
    build.tickets('CLOSED')
  table[.(1), nomatch = 0] %>%
    build.tickets('OPEN')
  table[.(4), nomatch = 0] %>%
    build.tickets('PENDING')
  table[.(5), nomatch = 0] %>%
    build.tickets('WORKING')
  build.tickets.raw()
} # FINISH

fetch.html.data.tickets.mt5ea <- function(report.phase1, get.open.fun, mysql.setting, timeframe.tickvalue,
                                          currency, symbols.setting) {
  ## 13 columns
  blocks <-
    suppressWarnings(
      readHTMLTable(report.phase1$PATH, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 2,
                    colClasses = c('character', num.char.to.num, toupper, toupper, toupper, num.char.to.num,
                                   num.char.to.num, num.char.to.num, rep('character', 5)))
    ) %>% as.data.table %>% html.data.mt5.table.blocks(c('Orders', 'Deals'))
  blocks$Orders %>% html.data.mt5.pending
  blocks$Deals %>% html.data.mt5.money_closed_open(get.open.fun, mysql.setting, timeframe.tickvalue,
                                                   currency, symbols.setting)
  build.tickets.raw()
} # FINISH

fetch.html.data.tickets.mt5trade <- function(report.phase1, get.open.fun, mysql.setting, timeframe.tickvalue,
                                             currency, symbols.setting) {
  ## 13 columns 
  blocks <-
    suppressWarnings(
      readHTMLTable(report.phase1$PATH, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1,
                    colClasses = c('character', num.char.to.num, toupper, toupper, toupper, num.char.to.num,
                                   num.char.to.num, num.char.to.num, rep('character', 5)))
    ) %>% as.data.table %>% html.data.mt5.table.blocks(c('Orders', 'Trade Positions', 'Working Orders', 'Deals'))
  blocks$Orders %>% html.data.mt5.pending
  trade.positions <- blocks$`Trade Positions`
  cprices <- trade.positions[, V9] %>% num.char.to.num %>% set_names(trade.positions$V3)
  blocks$`Working Orders` %>%
    setnames(c(1:9, 11), c('OTIME', 'TICKET', 'ITEM', 'TYPE', 'VOLUME',
                           'OPRICE', 'SL', 'TP', 'CPRICE', 'COMMENT')) %>%
    extract(j = c('VOLUME', 'CPRICE') := list(as.numeric(gsub(' / .*', '', VOLUME)),
                                              num.char.to.num(CPRICE))) %>%
    build.tickets('WORKING')
  blocks$Deals %>% html.data.mt5.money_closed_open(get.open.fun, mysql.setting, timeframe.tickvalue,
                                                   currency, symbols.setting, cprices)
  build.tickets.raw()
} # FINISH

html.data.mt5.pending <- function(orders.table) {
  orders.table %>% setnames(10, 'state') %>% setkey(state) %>% extract('canceled', nomatch = 0) %>%
    extract(j = c(10, 12, 13) := NULL) %>%
    setnames(1:10, c('OTIME', 'TICKET', 'ITEM', 'TYPE', 'VOLUME', 'OPRICE', 'SL', 'TP', 'CTIME', 'COMMENT')) %>%
    extract(j = c('CTIME', 'VOLUME') := list(time.char.to.posixct(CTIME), as.numeric(gsub(' / .*', '', VOLUME)))) %>%
    extract(j = c('SL', 'TP') := list(ifelse(is.na(SL), 0, SL), ifelse(is.na(TP), 0, TP))) %>%
    build.tickets('PENDING')
} # FINISH

html.data.mt5.money_closed_open <- function(deals.table, get.open.fun, mysql.setting, timeframe.tickvalue,
                                            currency, symbols.setting, cprices=NULL) {
  deals.table %<>% extract(j = c(2, 12) := NULL) %>%
    set_colnames(c('OTIME', 'ITEM', 'TYPE', 'direction', 'VOLUME', 'price', 'TICKET', 'COMMISSION',
                   'SWAP', 'PROFIT', 'COMMENT')) %>%
    extract(j = c('COMMISSION', 'SWAP') := list(num.char.to.num(COMMISSION), num.char.to.num(SWAP))) %>%
    setkey(TYPE)
  deals.table['BALANCE', .(TICKET = 0, OTIME, PROFIT = num.char.to.num(PROFIT)), nomatch = 0] %>%
    build.tickets('MONEY')
  deals.table[!'BALANCE',
              html.data.mt5.symbol.closed_open(.SD, ITEM, cprices[ITEM], get.open.fun, mysql.setting,
                                               timeframe.tickvalue, currency, symbols.setting),
              by = ITEM]
} # FINISH

html.data.mt5.symbol.closed_open <- function(deals.no.money.table, item, cprice, get.open.fun, mysql.setting,
                                             timeframe.tickvalue, currency, symbols.setting) {
  deals.no.money.table %<>% copy
  if (any('IN/OUT' %in% deals.no.money.table[, direction])) {
    deals.no.money.table %<>%
      setkey(OTIME) %>%
      extract(j = nVOLUME := {
        nvolume.change <- ifelse(TYPE == 'BUY', VOLUME, -VOLUME)
        abs(cumsum(nvolume.change))
      })
    in_out.tickets <-
      deals.no.money.table %>%
      setkey(direction) %>%
      extract('IN/OUT')
    new.in.tickets <-
      in_out.tickets %>%
      copy %>%
      extract(j = c('direction', 'VOLUME') := list('IN', nVOLUME))
    new.out.tickets <-
      in_out.tickets %>%
      copy %>%
      extract(j = c('direction', 'VOLUME') := list('OUT', VOLUME - nVOLUME))
    deals.no.money.table %<>%
      rbind(., new.in.tickets, new.out.tickets) %>%
      extract(j = nVOLUME := NULL)
  }
  deals.no.money.table %<>%
    setkey(direction)
  in.part <-
    deals.no.money.table['IN'] %>%
    extract(j = c('COMMISSION', 'SWAP', 'PROFIT', 'direction', 'ITEM') :=
              list(NULL, NULL, NULL, NULL, item)) %>%
    setnames('price', 'OPRICE') %>%
    setkey(TYPE)
  buy_in <- in.part['BUY', nomatch = 0]
  sell_in <- in.part['SELL', nomatch = 0]
  out.part <-
    deals.no.money.table['OUT'] %>%
    extract(j = direction := NULL) %>%
    setnames(c('OTIME', 'price'), c('CTIME', 'CPRICE')) %>%
    setkey(TYPE)
  buy_out <- out.part['BUY']
  sell_out <- out.part['SELL']
  if (nrow(buy_in)) {
    html.data.mt5.symbol.closed_open.build.tickets(buy_in, sell_out, cprice, get.open.fun, mysql.setting,
                                                   timeframe.tickvalue, currency, symbols.setting)
  }
  if (nrow(sell_in)) {
    html.data.mt5.symbol.closed_open.build.tickets(sell_in, buy_out, cprice, get.open.fun, mysql.setting,
                                                   timeframe.tickvalue, currency, symbols.setting)
  }
  NULL
} # FINISH

html.data.mt5.symbol.closed_open.build.tickets <- function(in.part, out.part, cprice, get.open.fun, mysql.setting,
                                                           timeframe.tickvalue, currency, symbols.setting) {
  in.part %<>%
    extract(j = c('csVOLUME', 'VOLUME') := list(cumsum(VOLUME), NULL)) %>%
    setkey(csVOLUME)
  out.part %<>%
    extract(j = c('csVOLUME', 'VOLUME') := list(cumsum(VOLUME), NULL)) %>%
    setkey(csVOLUME)
  volume.cs <-
    union(in.part[, csVOLUME], out.part[, csVOLUME]) %>%
    sort
  tickets.volume <- c(volume.cs[1], diff(volume.cs))
  tickets <- 
    data.table(
      VOLUME = tickets.volume,
      csVOLUME = volume.cs,
      SL = 0,
      TP = 0
    ) %>%
    setkey(csVOLUME) %>%
    out.part[., roll = -Inf] %>%
    in.part[., roll = -Inf]
  tickets[grep('sl', COMMENT), SL := CPRICE]
  tickets[grep('tp', COMMENT), TP := CPRICE]
  setkey(tickets, CPRICE)
  open <- tickets[is.na(CPRICE)] 
  closed <- tickets[!is.na(CPRICE)]
  if (nrow(open)) {
    open[j = c('CPRICE', 'COMMISSION', 'SWAP', 'PROFIT') := list(cprice, 0, 0, NA_real_)] %>% build.tickets('OPEN')
  }
  if (nrow(closed)) {
    symbol <- item.to.symbol(closed[1, ITEM], symbols.setting[, SYMBOL])
    if (!is.na(symbol)) {
      closed[j = PROFIT := {
        pips <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
        tickvalue <- cal.tick.value(symbol, CTIME, get.open.fun, mysql.setting, timeframe.tickvalue,
                                    currency, symbols.setting)
        cal.profit(VOLUME, tickvalue, pips)
      }]
    }
    build.tickets(closed, 'CLOSED')
  }
} # FINISH

html.data.mt5.table.blocks <- function(table, blocks) {
  column <- unlist(table[, 1, with = FALSE])
  table[, 1 := time.char.to.posixct(column), with = FALSE]
  space.index <- which(column == '')
  lapply(blocks, function(block) {
    block.index <- which(column == block)
    if (!length(block.index)) {
      return(NULL)
    }
    block.index.begin <- block.index + 2
    block.index.end <- space.index[which(space.index >= block.index.begin)[1]] - 1
    if (block.index.end < block.index.begin) {
      return(NULL)
    }
    table[block.index.begin:block.index.end]
  }) %>%
    set_names(blocks)
} # FINISH

fetch.html.data.tickets.mt4m_closed <- function(report.phase1) {
  suppressWarnings(
    readHTMLTable(report.phase1$PATH, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1,
                  colClasses = c('numeric', rep('character', 2), time.char.to.posixct, toupper, toupper,
                                 num.char.to.num, num.char.to.num, time.char.to.posixct, num.char.to.num, num.char.to.num,
                                 num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num,
                                 'character'))
  ) %>%
    as.data.table %>%
    extract(j = c('V2', 'V3', 'V13', 'V16', 'V17') := list(NULL, NULL, NULL, NULL, paste(V17, V2, sep = ' | '))) %>%
    extract(i = 2:(.N - 1)) %>%
    set_colnames(c('TICKET', 'OTIME', 'TYPE', 'ITEM', 'VOLUME', 'OPRICE', 'CTIME',
                   'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'COMMENT')) %>%
    build.tickets('CLOSED')
  build.tickets.raw()
} # FINISH

fetch.html.data.tickets.mt4m_raw <- function(report.phase1) {
  table <-
    suppressWarnings(
      readHTMLTable(report.phase1$PATH, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1,
                    colClasses = c('numeric', 'character', time.char.to.posixct, toupper, toupper, num.char.to.num,
                                   num.char.to.num, num.char.to.num, num.char.to.num, time.char.to.posixct,
                                   num.char.to.num, rep('character', 6), num.char.to.num, num.char.to.num,
                                   num.char.to.num, num.char.to.num, rep('character', 2)))
    ) %>%
    as.data.table %>%
    extract(j = c('V2', 'V12', 'V13', 'V14', 'V15', 'V16', 'V17', 'V22', 'V23') := 
              list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, paste(V23, V2, sep = ' | '))) %>%
    extract(i = 2:(.N - 7)) %>%
    set_colnames(c('TICKET', 'OTIME', 'TYPE', 'ITEM', 'VOLUME', 'SL', 'TP', 'OPRICE', 'CTIME',
                   'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'COMMENT')) %>%
    setkey(TYPE)
  table['BALANCE'] %>%
    extract(j = .(TICKET, OTIME, PROFIT = VOLUME)) %>%
    build.tickets('MONEY')
  table[c('BUY LIMIT', 'BUY STOP', 'SELL LIMIT', 'SELL STOP')] %>%
    build.tickets('PENDING')
  table[c('BUY', 'SELL')] %>%
    build.tickets('CLOSED')
  build.tickets.raw()
} # FINISH


#### CALCULATION ####
symbol.base.currency <- function(symbol) {
  # ''' symbol's base currency '''
  # 2017-01-23: Version 1.0
  substr(symbol, 4, 6)
} # FINISH

symbol.quote.currency <- function(symbol) {
  # ''' symbol's quote currency '''
  # 2017-01-23: Version 1.0
  substr(symbol, 1, 3)
} # FINISH

build.symbol <- function(currency1, currency2, support.symbols=SUPPORT.SYMBOLS) {
  # ''' build symbol from 2 currencies '''
  # 2016-08-12: Version 1.0 FIX support.symbols: self$get.support.symbols() should usd all symbol table
  match.currency1.symbols <-
    support.symbols %>%
    extract(support.symbols %>% str_detect(currency1))
  match.currency1.symbols %>%
    extract(match.currency1.symbols %>% str_detect(currency2)) %>% {
      if (length(.) == 1) {
        .
      } else {
        ''
      }
    }
} # FINISH

cal.tick.value = function(symbol, times, get.open.fun, mysql.setting, timeframe='M1', currency=DEFAULT.CURRENCY,
                          symbols.setting=SYMBOLS.SETTING) {
  # ''' cal tick.value '''
  # 2017-01-23: Version 0.1
  base.currency <- symbol.base.currency(symbol)
  tick.value.point <- symbols.setting[symbol, TICKVALUE.POINT]
  if (base.currency == currency) {
    return(tick.value.point)
  }
  target.symbol <- build.symbol(base.currency, currency, symbols.setting[j = SYMBOL])
  if (target.symbol == '') {
    return(1)
  }
  target.open <- get.open.fun(target.symbol, times, timeframe, mysql.setting)
  if (base.currency == symbol.base.currency(target.symbol)) {
    return(tick.value.point / target.open)
  } else {
    return(tick.value.point * target.open)
  }
}

cal.margin.required = function(symbol, times, get.open.fun, mysql.setting, timeframe='M1', currency=DEFAULT.CURRENCY, 
                               leverage=DEFAULT.LEVERAGE, symbols.setting=SYMBOLS.SETTING) {
  # ''' cal margin required '''
  # 2017-01-23: Version 0.1
  quote.currency <- symbol.quote.currency(symbol)
  margin.required.point <- symbols.setting[symbol, CONTRACT.SIZE] / leverage
  if (quote.currency == currency) {
    return(margin.required.point)
  }
  target.symbol <- build.symbol(quote.currency, currency, symbols.setting[j = SYMBOL])
  if (target.symbol == '') {
    return(1000)
  }
  target.open <- get.open.fun(target.symbol, times, timeframe, mysql.setting)
  if (quote.currency == symbol.quote.currency(target.symbol)) {
    return(margin.required.point * target.open)
  } else {
    return(margin.required.point / target.open)
  }
}

cal.profit <- function(volume, tickvalue, pips) {
  # ''' calculate profit from: volume, tickvalue, pips '''
  # 2016-08-15: Version 1.0
  round(volume * tickvalue * pips, 2)
}

cal.pips <- function(type, open.price, close.price, digit) {
  # ''' calculate pips (V) '''
  # 2017-02-10: Version 1.1 ifelse mode
  # 2017-01-22: Version 1.0
  ifelse(grepl('buy', type, ignore.case = TRUE), close.price - open.price, open.price - close.price) %>%
    multiply_by(10 ^ digit)
} # FINISH

#### BUILD TICKETS ####
TICKETS.COLUMNS <- list(
  # UNIFORM = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
  #             'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'COMMENT', 'GROUP', 'EXIT'),
  MONEY = c('TICKET', 'OTIME', 'CTIME', 'PROFIT'),
  CLOSED = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
             'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'EXIT'),
  OPEN = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
           'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT'),
  PENDING = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
              'CTIME', 'CPRICE'),
  WORKING = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP', 'CPRICE')
)

TICKETS.TEMPLATE <- data.table(
  TICKET = NA_integer_,
  OTIME = ymd_hms(NA, tz = 'GMT', quiet = TRUE),
  TYPE = NA_character_,
  VOLUME = NA_real_,
  ITEM = NA_character_,
  OPRICE = NA_real_,
  SL = NA_real_,
  TP = NA_real_,
  CTIME = ymd_hms(NA, tz = 'GMT', quiet = TRUE),
  CPRICE = NA_real_,
  COMMISSION = NA_real_,
  TAXES = NA_real_,
  SWAP = NA_real_,
  PROFIT = NA_real_,
  COMMENT = NA_character_,
  GROUP = NA_character_,
  EXIT = NA_character_
) %>% extract(TYPE == 'MAKE EMPTY TABLE')

build.tickets = function(table, group) {
  # ''' build tickets '''
  # 2017-01-21: Version 1.1 add 'EXIT' in Closed & Uniform
  # 2017-01-21: Version 1.0 change logic expr to || short way connection
  # 2017-01-17: Version 0.2 add Comment
  # 2017-01-17: Version 0.1
  if (is.null(table) || !nrow(table)) {
    return(NULL) 
  }
  table.columns <- colnames(table)
  table[, GROUP := group]
  ## default 0 check
  zero.columns <- setdiff(TICKETS.COLUMNS[[group]], table.columns)
  if (length(zero.columns)) {
    table[, (zero.columns) := list(0)]
  }
  if (group == 'CLOSED' & 'COMMENT' %in% table.columns) {
    table[, EXIT := comment.to.exit(COMMENT)]
  }
  if (group == 'MONEY') {
    table[, CTIME := OTIME]
  }
  append.to.tickets.temp(table)
} # FINISH

comment.to.exit <- function(comments) {
  # ''' comment to exit '''
  # 2017-02-09: Version 1.2 add ignore.case argument, need not to toupper()
  # 2017-01-17: Version 1.1 add support for comments type - data.frame
  # 2016-12-01: Version 1.0
  comments %<>% gsub('/| / ', '', .)
  exit <- rep(NA_character_, length(comments))
  exit[grep('SO', comments, ignore.case = TRUE)] <- 'SO'
  exit[grep('SL', comments, ignore.case = TRUE)] <- 'SL'
  exit[grep('TP', comments, ignore.case = TRUE)] <- 'TP'
  exit
} # FINISH

#### FORMAT COLCLASSES ####
time.char.to.posixct <- function(time.char) {
  new.time <- ymd_hms(rep(NA, length(time.char)), tz = 'GMT', quiet = TRUE) 
  len <- nchar(time.char)
  index.19 <- which(len == 19)
  index.16 <- union(which(len == 16), grep(',', time.char))
  index.10 <- which(len == 10)
  if (length(index.19)) {
    new.time[index.19] <- ymd_hms(time.char[index.19], tz = 'GMT')
  }
  if (length(index.16)) {
    new.time[index.16] <- ymd_hm(time.char[index.16], tz = 'GMT')
  }
  if (length(index.10)) {
    new.time[index.10] <- ymd(time.char[index.10], tz = 'GMT')
  }
  new.time
} # FINISH

num.char.to.num <- function(num.char) {
  as.numeric(gsub(' ', '', num.char))
} # FINISH

#### ENVIRONMENT ####
.METAQUOTE.ANALYTIC <- new.env(parent = baseenv())
.METAQUOTE.ANALYTIC$.TICKETS.TEMP <- list()

init.tickets.temp <- function() {
  .METAQUOTE.ANALYTIC$.TICKETS.TEMP <- list()
}

append.to.tickets.temp <- function(tickets) {
  .METAQUOTE.ANALYTIC$.TICKETS.TEMP %<>% c(list(tickets))
} # FINISH

build.tickets.raw <- function() {
  tickets <- .METAQUOTE.ANALYTIC$.TICKETS.TEMP %>% append(list(TICKETS.TEMPLATE), .) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    extract(
      j = (names(TICKETS.TEMPLATE)),
      with = FALSE
    ) %>% setkey(CTIME)
  init.tickets.temp()
  tickets
}

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

#### OTHERS ####
item.to.symbol <- function(item, support.symbols) {
  # ''' item to symbol '''
  if (is.na(item) || item == '' || grepl('^BX', item, ignore.case = TRUE)) {
    return(NA_character_) 
  }
  support.symbols %>%
    extract(support.symbols %>% str_detect(item)) %>% {
      if (length(.) == 1) {
        .
      } else {
        NA_character_
      }
    }
}

item.symbol.mapping <- function(tickets.raw, support.symbols) {
  unique(tickets.raw[GROUP != 'MONEY', ITEM]) %>%
    sapply(item.to.symbol, support.symbols)
} # FINISH

unsupported.items <- function(item.symbol.mapping) {
  names(item.symbol.mapping) %>%
    extract(item.symbol.mapping %>% is.na %>% which)
} # FINISH

supported.items <- function(item.symbol.mapping) {
  names(item.symbol.mapping) %>%
    extract(item.symbol.mapping %>% is.na %>% not %>% which)
} # FINISH


