library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####

#### @PATCH NOTE@ ####
## 2017-03-26: Version 0.1



html.report.phase3 <- function(html.report.phase2, get.open.fun=DB.O, timeframe.tickvalue='M1', 
                               symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING) {
  
  within(html.report.phase2, {
    tickets.edited(TICKETS.EDIT, CURRENCY, get.open.fun, timeframe.tickvalue, symbols.setting, mysql.setting)
    TICKETS.MONEY <- tickets.money(TICKETS, set.init.money, include.middle, default.money)
    
    PERIOD <- tickets.period(TICKETS.EDIT)
    
    
    PHASE <- 3
  })
}


tickets.edited <- function(tickets.editing, currency, get.open.fun=DB.O, timeframe.tickvalue='M1', 
                           symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING) {
  tickets.editing %>%
    setkey(SYMBOL) %>%
    extract(j = c('PIP', 'PROFIT', 'NPROFIT') := {
      symbol <- SYMBOL[1]
      pip <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
      tickvalue <- cal.tick.value(symbol, CTIME, get.open.fun, mysql.setting, timeframe.tickvalue,
                                  currency, symbols.setting)
      profit <- cal.profit(VOLUME, tickvalue, pip)
      list(round(pip, 0), profit, COMMISSION + TAXES + SWAP + PROFIT)
    }, by = SYMBOL) %>%
    extract(j = c('LOT.PROFIT', 'PL') :=
              list(PROFIT / VOLUME, ifelse(NPROFIT >= 0, 'PROFIT', 'LOSS')))
} # FINISH