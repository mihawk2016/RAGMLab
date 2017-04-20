## Hypothsis
Hypothesis <- function(tickets, includeopen = FALSE, ctimezone = c('Asian', 'Europe', 'USA'), ctzreverse = FALSE, csymbol = c(), csreverse = FALSE, cvolumerange = c(0.00, 5.00), resize = 0, reverse = FALSE) {
  ## get trades
  groups <- tickets$Group
  closed.index <- which(groups == 'CLOSED')
  if(includeopen) open.index <- which(groups == 'OPEN')
  else open.index <- NULL
  trades.index <- c(closed.index, open.index)
  trades <- tickets[trades.index, ]
  ## trades add col
  trades <- by(trades, trades$Item, function(x) {
    pips <- (x$CPrice - x$OPrice) * 10^SYMBOLS[as.character(x$Item[1]), 'Digit']
    tickvalues <- abs(x$Profit / pips / x$Volume)
    x$TickValue <- tickvalues
    x
  })
  trades <- do.call(rbind, trades)
  ## Hypothesis choose timezone
  trades <- H.Choose.TimeZone(trades, timezone = ctimezone, reverse = ctzreverse)
  trades <- H.Choose.Symbol(trades, symbol = csymbol, reverse = csreverse)
  trades <- H.Choose.Volume(trades, range = cvolumerange)
  trades <- H.Resize(trades, volume = resize)
  if(reverse) trades <- H.Reverse(trades)
  trades
}

## Hypothesis choose timezone = c('Asian', 'Europe', 'USA')
H.Choose.TimeZone <- function(trades, timezone = c('Asian', 'Europe', 'USA'), reverse = FALSE) {
  len <- length(timezone)
  if(len == 3) return(trades)
  if(len == 0) return(NULL)
  ## get numeric time
  times <- as.numeric(trades$OTime) %% 86400
  indices <- lapply(timezone, function(x) {
    from <- TIMEZONE[x, 'FROM']
    to <- TIMEZONE[x, 'TO']
    which(CheckTime(times, from, to))
  })
  indices <- do.call('c', indices)
  if(reverse) trades <- trades[-indices, ]
  else trades <- trades[indices, ]
  trades
}
## check time in from - to
CheckTime <- function(time, from, to) {
  if(from < to) return(time >= from & time < to)
  time >= from | time < to
}
## time string to sec int
TimeStrToSec <- function(timestr) {
  as.numeric(strptime(timestr ,'%H:%M')) %% 86400
}

## Hypothesis choose symbols = c()
H.Choose.Symbol <- function(trades, symbol = c(), reverse = FALSE) {
  if(is.null(trades) | length(symbol) == 0) return(trades)
  ## symbols indices
  items <- trades$Item
  indices <- lapply(symbol, function(x) {
    which(items == x)
  })
  indices <- do.call('c', indices)
  if(reverse) trades <- trades[-indices, ]
  else trades <- trades[indices, ]
  trades
}


## Hypothesis Resize
H.Resize <- function(trades, volume = 0) {
  if(is.null(trades) | volume == 0) return(trades)
  volumes <- trades$Volume
  n <- volume / volumes
  trades$Volume <- volume
  trades$Commission <- trades$Commission * n
  trades$Taxes <- trades$Taxes * n
  trades$Swap <- trades$Swap * n
  trades
}

## Hypothesis Reverse
H.Reverse <- function(trades) {
  if(is.null(trades)) return(trades)
  ## split for buy and sell group
  typelist <- split(trades, trades$Type)
  buys <- typelist$BUY
  sells <- typelist$SELL
  n.buys <- nrow(buys)
  n.sells <- nrow(sells)
  ## edit BUYs
  if(!is.null(n.buys)) {
    buys$Type <- as.factor('SELL')
    buys <- by(buys, buys$Item, function(x) {
      symbol <- as.character(x$Item[1])
      spread <- SYMBOLS[symbol, 'Spread']
      digit <- SYMBOLS[symbol, 'Digit']
      diff <- spread * 10^(-digit)
      x$OPrice <- round(x$OPrice - diff, digit)
      x$CPrice <- round(x$CPrice + diff, digit)
      x
    })
    if(n.buys > 1) buys <- do.call(rbind, buys)
    else buys <- buys[[1]]
  }
  ## edit SELLs
  if(!is.null(n.sells)) {
    sells$Type <- as.factor('BUY')
    sells <- by(sells, sells$Item, function(x) {
      symbol <- as.character(x$Item[1])
      spread <- SYMBOLS[symbol, 'Spread']
      digit <- SYMBOLS[symbol, 'Digit']
      diff <- spread * 10^(-digit)
      x$OPrice <- round(x$OPrice + diff, digit)
      x$CPrice <- round(x$CPrice - diff, digit)
      x
    })
    if(n.sells > 1) sells <- do.call(rbind, sells)
    else sells <- sells[[1]]
  }
  ## into list
  rbind(buys, sells)
}

## Hypothesis split 【分离成包含多个Records的列表】
