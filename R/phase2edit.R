library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####

#### @PATCH NOTE@ ####
## 2017-03-26: Version 0.1




tickets.edit.init <- function(report) {
  report$TICKETS %>% extract(i = GROUP == 'CLOSED' & !is.na(SYMBOL))
} # FINISH


report.edit.reset <- function(report) {
  if (report$PHASE < 3) {
    return(report)
  }
  within(report, {
    ## TODO delete some Phase 3 element
    
    TICKETS.GROUP.EDIT <- DEFAULT.TICKETS.GROUP.EDIT
    TICKETS.EDIT.HISTORY <- c()
    TICKETS.EDIT <- TICKETS[GROUP == 'CLOSED' & !is.na(SYMBOL)]
    PHASE <- 2
  })
}

report.edit.focus.tickets <- function(report) {
  ifelse(is.null(report$TICKETS.EDIT), tickets.edit.init(report), report$TICKETS.EDIT)
}

#### EDIT GROUP ####
report.edit.money.init <- function(report, mony.init=NULL) {
  within(report, {
    TICKETS.GROUP.EDIT$MONEY.INIT <- money.init
  })
}

report.edit.money.middle <- function(report, include=TRUE) {
  within(report, {
    TICKETS.GROUP.EDIT$MONEY.MIDDLE.INCLUDE <- include
  })
}

report.edit.open <- function(report, open='INCLUDE', get.open.fun=DB.O, timeframe.tickvalue='M1', 
                             symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING) {
  if (report$TICKETS.GROUP.EDIT$OPEN == open) {
    return(report)
  }
  tickets <- report.edit.focus.tickets(report)
  within(report, {
    TICKETS <- switch(
      open,
      'CLOSE' = rbind(
        tickets[GROUP == 'CLOSED'],
        report.tickets.close.the.open(TICKETS[GROUP == 'OPEN'], CURRENCY, get.open.fun, timeframe.tickvalue,
                                      symbols.setting, mysql.setting),
        use.names = TRUE, fill = TRUE),
      tickets[GROUP == 'CLOSED']
    )
    TICKETS.GROUP.EDIT$OPEN <- open
  })
}

report.edit.pending <- function(report, include=TRUE) {
  within(report, {
    TICKETS.GROUP.EDIT$PENDING.INCLUDE <- include
  })
}

report.edit.working <- function(report, include=TRUE) {
  within(report, {
    TICKETS.GROUP.EDIT$WORKING.INCLUDE <- include
  })
}

report.tickets.close.the.open <- function(open, currency, get.open.fun=DB.O, timeframe.tickvalue='M1', 
                                          symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING) {
  open %>%
    setkey(SYMBOL) %>%
    extract(
      j = PROFIT := {
        symbol <- SYMBOL[1]
        pip <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
        tickvalue <- cal.tick.value(symbol, CTIME, get.open.fun, mysql.setting, timeframe.tickvalue,
                                    currency, symbols.setting)
        cal.profit(VOLUME, tickvalue, pip)
      },
      by = SYMBOL
    )
}

