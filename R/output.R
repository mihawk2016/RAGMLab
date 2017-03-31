library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####

#### @PATCH NOTE@ ####
## 2017-03-29: Version 0.1


output.report.html <- function(report.phase3, title='MetaQuote Analytics') {
  file.name <- './tests/test.html'
  file.create(file.name)
  body.tags <- output.report.html.body(report.phase3, title)
  
  os <- Sys.info()['sysname']
  if (os == 'Windows') {
    Sys.setlocale(locale = 'us')
    writeLines(output.report.html.template(title, body.tags) %>% as.character, file(file.name))
    Sys.setlocale(locale = 'Chinese')
  } else if (os == 'Linux') {
    Sys.setlocale(locale = 'en_US.UTF-8')
    writeLines(output.report.html.template(title, body.tags) %>% as.character, file(file.name))
    Sys.setlocale(locale = 'zh_CN.UTF-8')
  } else {
    NULL
  }
}

output.report.html.body <- function(report.phase3, title='MetaQuote Analytics') {
  with(report.phase3, {
    infos.table <- INFOS %>% htmlTable(
      rnames = F,
      css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
      css.cell = rep('width: 150px;', 8),
      col.rgroup = c("none", "#F9FAF0"),
      col.columns = c("none", "#F1F0FA")) %>% HTML
    
    TIMESERIE.ACCOUNT %>% extract(j = .(TIME, EQUITY, MARGIN.FREE, MARGIN.USED)) %>%
      melt('TIME') %>% {
        ggplot(.) +
          geom_line(aes(x = TIME, y = value, color = variable)) +
          facet_grid(variable ~ ., scales = 'free_y') +
          xlab('EQUITY & MARGIN') +
          ylab(NULL) +
          theme(legend.position = 'none', axis.ticks.x = element_blank()) +
          scale_y_continuous(labels = scales::dollar)
      }
    equity_margin.img <- plot.to.tag.img()
    
    trade.period <- with(PERIOD, {
      data.table(
        START = int_start(NATURE.INTERVAL),
        NATURAL.PERIOD = as.character(NATURE.PERIOD),
        TRADE.DAYS = TRADE.DAYS,
        END = int_end(NATURE.INTERVAL))
    }) %>% htmlTable(
      rnames = F,
      caption = 'TRADE PERIOD',
      css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
      css.cell = rep('width: 300px;', 4),
      col.rgroup = c("none", "#F9FAF0"),
      col.columns = c("none", "#F1F0FA")) %>% HTML
    
    lables <- c(BALANCE.DELTA = 'PROFIT', NET.VOLUME = 'VOLUME')
    melt(SYMBOLS.PROFIT_VOLUME, c('TIME', 'SYMBOL')) %>% {
      ggplot(.) +
        geom_line(aes(x = TIME, y = value, color = SYMBOL)) +
        facet_grid(variable ~ ., scales = 'free_y', labeller = labeller(variable = lables)) +
        xlab('PROFIT & VOLUME') +
        ylab(NULL) +
        theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.key.size = unit(10, 'pt'),
              legend.title = element_blank())
    }
    profit_volume.img <- plot.to.tag.img()
    
    calendar.yield <- yield.calendar(TIMESERIE.ACCOUNT) %>% htmlTable(
      rnames = F,
      caption = 'YIELD CALENDAR',
      css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
      css.cell = rep('width: 85px;', 14),
      col.rgroup = c("none", "#F9FAF0"),
      col.columns = c("none", "#F1F0FA")) %>% HTML
    
    calendar.mddp <- mddp.calendar(TIMESERIE.ACCOUNT) %>% htmlTable(
      rnames = F,
      caption = 'MAX-DRAWDOWN CALENDAR (to EQUITY)',
      css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
      css.cell = rep('width: 85px;', 14),
      col.rgroup = c("none", "#F9FAF0"),
      col.columns = c("none", "#F1F0FA")) %>% HTML
    
    calendar.b.mddp <- balance.mddp.calendar(TICKETS.EDIT, TICKETS.MONEY) %>% htmlTable(
      rnames = F,
      caption = 'MAX-DRAWDOWN CALENDAR (to BALANCE)',
      css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
      css.cell = rep('width: 85px;', 14),
      col.rgroup = c("none", "#F9FAF0"),
      col.columns = c("none", "#F1F0FA")) %>% HTML
    
    net.profit <- STATISTIC.ACCOUNT.PL[PL == 'TOTAL', SUM]
    con.monthly.loss <-
      TIMESERIE.ACCOUNT %>%
      setkey(TIME) %>%
      extract(
        j = .(TIME, BALANCE.DELTA = c(0, BALANCE.DELTA) %>% diff, YM = format(TIME, '%Y%m'))
      ) %>%
      extract(
        j = .(M.PROFIT = sum(BALANCE.DELTA)),
        by = YM
      ) %>%
      extract(j = M.PROFIT) %>% cal.continuous %>% {
        if (length(.$DN.TO)) {
          (.$DN.TO - .$DN.FROM + 1) %>% max
        } else {
          0
        }
      }
    trades.per.day <- STATISTIC.ACCOUNT.OTHERS[ITEM == 'TRADE', DAILY]
    mdd.portfolio <- STATISTIC.ACCOUNT.OTHERS[MDD == 'MDDP', MDD.VALUE]
    win.percent <-
      STATISTIC.ACCOUNT.PL[, SUM] %>% {
        (.)[1] / (.)[3] * 100
      } %>% round(2)
    threshold <- c(0, 2, 1, 30, 50)
    result.char <- c('FAIL', 'OK')
    evaluation <- data.table(
      THEME = c('Net Profit',
                'Monthly Yield',
                'Trades per Day',
                'Max Drawdown Percent',
                'Win Percent'),
      DESCRIPTION = c(sprintf(' greater than %.2f $', threshold[1]),
                      sprintf(' no more than %i Loss in a row', threshold[2]),
                      sprintf(' greater than %.2f trades per day', threshold[3]),
                      sprintf(' less than %.2f %%', threshold[4]),
                      sprintf(' greater than %.2f %%', threshold[5])),
      RESULT = c(sprintf('%.2f', net.profit),
                 sprintf('max %i', con.monthly.loss),
                 sprintf('%.2f', trades.per.day),
                 sprintf('%.2f %%', mdd.portfolio),
                 sprintf('%.2f %%', win.percent)),
      EVALUATION = result.char[c(net.profit > threshold[1],
                                 con.monthly.loss <= threshold[2],
                                 trades.per.day > threshold[3],
                                 mdd.portfolio < threshold[4],
                                 win.percent > threshold[5]) + 1]
    ) %>% htmlTable(
      rnames = F,
      css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
      css.cell = rep('width: 300px;', 4),
      col.rgroup = c("none", "#F9FAF0"),
      col.columns = c("none", "#F1F0FA")) %>% HTML
    
    yearly.yield <- STATISTIC.ACCOUNT.OTHERS[ITEM == 'YIELD', YEARLY]
    yield.score <- score.yearly.return(yearly.yield)
    mddp <- STATISTIC.ACCOUNT.OTHERS[MDD == 'MDDP', MDD.VALUE]
    mddp.score <- score.mddp(mddp)
    score <- mean(c(yield.score, mddp.score)) %>% round(0)
    scores <- data.table(
      YEARLY.YIELD = sprintf('%s', yearly.yield),
      YIELD.SCORE = sprintf('(%s)', yield.score %>% round(0)),
      SCORE.LEVEL = sprintf('%s (%s)', level(score), score),
      MDDP.SCORE = sprintf('(%s)', mddp.score %>% round(0)),
      MDDP.YIELD = sprintf('%s', mddp)
    ) %>% htmlTable(
      rnames = F,
      css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
      css.cell = rep('width: 192px;', 5),
      cgroup = c('YEARLY.RETURN', 'LEVEL', 'MAX.DRAWDOWN'),
      n.cgroup = c(2, 1, 2),
      col.rgroup = c("none", "#F9FAF0"),
      col.columns = c(
        rep("none", 2),
        "#F1F0FA",
        rep("none", 2))
      ) %>% HTML
    
    account.pl <- copy(STATISTIC.ACCOUNT.PL) %>%
      setnames(c('P/L', 'N', 'SUM', 'MEAN', 'MAX', 'SUM', 'MEAN', 'MAX', 'SUM', 'MEAN', 'MAX', 'MIN', 'N.MAX',
                 'N.MEAN', 'MAX', 'MEAN')) %>%
      extract(
        j = N := {
          win.percent <- N[c(TRUE, FALSE, FALSE)] / N[c(FALSE, FALSE, TRUE)] * 100
          char.n <- as.character(N)
          char.n[c(TRUE, FALSE, FALSE)] <- sprintf('%s (%.2f%%)', char.n[c(TRUE, FALSE, FALSE)], win.percent)
          char.n
        }
      ) %>% htmlTable(
        rnames = F,
        caption = 'PROFIT vs LOSS',
        css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
        css.cell = rep('width: 75px;', 16),
        cgroup = c('', '', 'MONEY', 'PIP', 'VOLUME', 'CONTINUOUS'),
        n.cgroup = c(1, 1, 3, 3, 4, 4),
        col.rgroup = c("none", "#F9FAF0"),
        col.columns = c("none", "#F1F0FA")) %>% HTML
    
    account.others <- copy(STATISTIC.ACCOUNT.OTHERS) %>%
      extract(
        j = MDD.PERIOD := time.num.to.period.char(MDD.PERIOD)
      ) %>% htmlTable(
        rnames = F,
        caption = 'OTHERS',
        css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
        css.cell = rep('width: 64px;', 15),
        cgroup = c('PROF.YIELD.RETURN', 'EXIT', 'SUMMARY', 'MAX.DRAWDOWN'),
        n.cgroup = c(6, 4, 2, 3),
        col.rgroup = c("none", "#F9FAF0"),
        col.columns = c(
          rep("none", 6),
          rep("#F1F0FA", 4),
          rep("none", 2),
          rep("#F1F0FA", 3))) %>% HTML
    
    n.symbols <- length(SYMBOLS)
    symbol.report <-
      if (n.symbols < 2) {
        NULL
      } else {
        symbol.pl <- copy(STATISTIC.SYMBOLS.PL) %>%
          extract(j = !c(1), with = FALSE) %>%
          setnames(c('PL', 'N', 'SUM', 'MEAN', 'MAX', 'SUM', 'MEAN', 'MAX', 'SUM', 'MEAN', 'MAX', 'MIN', 'N.MAX', 
                     'N.MEAN', 'MAX', 'MEAN')) %>%
          extract(
            j = N := {
              win.percent <- N[c(TRUE, FALSE, FALSE)] / N[c(FALSE, FALSE, TRUE)] * 100
              char.n <- as.character(N)
              char.n[c(TRUE, FALSE, FALSE)] <- sprintf('%s (%.2f)', char.n[c(TRUE, FALSE, FALSE)], win.percent)
              char.n
            }) %>%
          htmlTable(
            rnames = F,
            caption = 'PROFIT vs LOSS',
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            css.cell = rep('width: 75px;', 16),
            rgroup = SYMBOLS,
            n.rgroup = rep(3, n.symbols),
            cgroup = c('', '', 'MONEY', 'PIP', 'VOLUME', 'CONTINUOUS'),
            n.cgroup = c(1, 1, 3, 3, 4, 4),
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c("none", "#F1F0FA")) %>% HTML
        
        symbol.others <- copy(STATISTIC.SYMBOLS.OTHERS) %>%
          extract(j = !c(1), with = FALSE) %>%
          extract(
            j = MDD.PERIOD := time.num.to.period.char(MDD.PERIOD)
          ) %>%
          htmlTable(
            rnames = F,
            caption = 'OTHERS',
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            rgroup = SYMBOLS,
            n.rgroup = rep(3, n.symbols),
            css.cell = rep('width: 64px;', 15),
            cgroup = c('PROF.YIELD.RETURN', 'EXIT', 'SUMMARY', 'MAX.DRAWDOWN'),
            n.cgroup = c(6, 4, 2, 3),
            col.rgroup = c("none", "#F9FAF0"),
            col.columns = c(
              rep("none", 6),
              rep("#F1F0FA", 4),
              rep("none", 2),
              rep("#F1F0FA", 3))) %>% HTML
        
        SYMBOLS.RETURN %>%
          extract(j = !c('TIME'), with = FALSE) %>%
          myAssetsCorImagePlot(abbreviate = 10)
        symbols.cor.img <- plot.to.tag.img()
        
        tagList(
          tags$h2('Symbol Report'),
          symbol.pl,
          symbol.others,
          symbols.cor.img
        )
      }
    
    tickets <- copy(TICKETS.EDIT) %>%
      setkey(OTIME) %>%
      extract(
        j = c('VOLUME', 'PERIOD', 'MFPP', 'MFLP', 'NPROFIT') := list(round(VOLUME, 2), PERIOD, MFPP, MFLP,
                                                                     round(NPROFIT, 2))
      ) %>%
      extract(
        j = c(
          'TICKET', 'ITEM', 'TYPE', 'VOLUME', 'OTIME', 'OPRICE', 'CTIME', 'CPRICE', 'SL', 'TP',
          'NPROFIT', 'PIP', 'MFP', 'MFPP', 'MFL', 'MFLP', 'PERIOD', 'EXIT'),
        with = FALSE
      ) %>%
      setnames(
        c('Ticket', 'Item', 'Type', 'Vol', 'Time', 'Price', 'Time', 'Price',' SL', 'TP',
          'NetProf', 'Pip', 'Prof', 'Prof.P', 'LOSS', 'Loss.P', 'Period', 'Exit')) %>%
      htmlTable(
        rnames = F,
        css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
        cgroup = c('', 'REQUEST', 'OPEN', 'CLOSE', 'STOPS', 'RESULT', 'MAX FLOATING', ''),
        n.cgroup = c(1, 3, 2, 2, 2, 2, 4, 2),
        col.rgroup = c("none", "#F9FAF0"),
        col.columns = c("none", "#F1F0FA")) %>% HTML
    div(
      class = 'main-container',
      tagList(
        tags$h1(class = 'title', title),
        tags$hr(),
        tags$h2('Overview'),
        infos.table,
        equity_margin.img,
        trade.period,
        profit_volume.img,
        tags$h2('Calendar'),
        calendar.yield,
        calendar.mddp,
        calendar.b.mddp,
        tags$h2('Evalution'),
        evaluation,
        tags$h2('Scores'),
        scores,
        tags$h2('Portfolio Report'),
        account.pl,
        account.others,
        symbol.report,
        tags$h2('Tickets Report'),
        tickets
      )
    )
  })
}

output.report.html.template.css <- function() {
  tags$style(
    tagList(
      css.builder('body, td', 'font-size' = '10px', 'font-family' = '"Comic Sans MS", cursive, sans-serif'),
      css.builder('main-container', 'class', width = '960px', 'margin-left' = 'auto', 'margin-right' = 'auto'),
      css.builder('h1.title', 'text-align' = 'center', 'font-size' = '38px'),
      css.builder('h2', 'font-size' = '30px')
    )
  )
}

#### HTML TEMPLATE ####
output.report.html.template <- function(title='MetaQuote Analytics', body.tags=NULL) {
  tagList(
    HTML('<!DOCTYPE html>'),
    tags$html(
      tagList(
        output.report.html.template.head(title),
        output.report.html.template.body(body.tags)
      )
    )
  )
}

output.report.html.template.head <- function(title='MetaQuote Analytics') {
  tagList(
    HTML('<head>'),
    tags$meta(charset='utf-8'),
    tags$title(title),
    output.report.html.template.css(),
    HTML('</head>')
  )
}

output.report.html.template.body <- function(body.tags) {
  tags$body(
    body.tags
  )
}

css.builder <- function(selector, selector.type='tag', ...) {
  selector.line <-
    (if (length(selector) == 1) {
      switch(
        selector.type,
        'id' = '#',
        'class' = '.',
        'tag' = ''
      ) %>% paste0(selector)
    } else if (length(selector) == 2){
      paste(selector, collapse = '.')
    }) %>%
    paste0(' {')
  declarations <- css(..., collapse_ = '\n  ')
  last.line <- '}'
  paste(selector.line, paste0('  ', declarations), last.line, sep = '\n') %>% HTML
}

plot.to.tag.img <- function(width = 960) {
  file <- tempfile()
  suppressMessages(ggsave(file, device = 'png', width = width / 25, units = 'cm'))
  img.tag <- tags$img(src = image_uri(file), width = '100%')
  unlink(file)
  img.tag
}


#### OTHERS ####
yield.calendar <- function(dt, percent=TRUE, digits=2) {
  setkey(dt, TIME)
  tb <- dt[, .(TIME, RETURN, YM = format(TIME, '%Y%m'), Y = format(TIME, '%Y'))]
  years <- year(tb[1, TIME]):year(tb[.N, TIME])
  calendar <- matrix(data = NA, nrow = length(years), ncol = 13,
                     dimnames = list(years, c(strftime(seq.Date(as.Date("2000-01-01"), length.out = 12, by = "months"),
                                                       format = "%b"), 'TOTAL')))
  tb[, {
    year <- substr(YM[1], 1, 4)
    month <- as.numeric(substr(YM[1], 5, 6))
    calendar[year, month] <<- cum.return(RETURN) %>%
      extract(length(.))
  }, by = YM]
  tb[, {
    calendar[Y[1], 13] <<- cum.return(RETURN) %>%
      extract(length(.))
  }, by = Y]
  calendar %>%
    as.data.table(keep.rownames=TRUE) %>%
    setnames(1, 'YIELD(%)')
} # FINISH

mddp.calendar <- function(dt, percent=TRUE, digits=2) {
  setkey(dt, TIME)
  tb <- dt[, .(TIME, RETURN, YM = format(TIME, '%Y%m'), Y = format(TIME, '%Y'))]
  years <- year(tb[1, TIME]):year(tb[.N, TIME])
  calendar <- matrix(data = NA, nrow = length(years), ncol = 13,
                     dimnames = list(years, c(strftime(seq.Date(as.Date("2000-01-01"), length.out = 12, by = "months"),
                                                       format = "%b"), 'TOTAL')))
  tb[, {
    year <- substr(YM[1], 1, 4)
    month <- as.numeric(substr(YM[1], 5, 6))
    RETURN[is.na(RETURN)] <- 0
    return.serie <- cumprod(RETURN + 1)
    calendar[year, month] <<-
      return.serie %>%
      maxdrawdown %>% {
        1 - return.serie[.$TO][1] / return.serie[.$FROM][1]
      } %>% {
        if (percent) {
          (.) %>% multiply_by(100)
        } else {
          .
        }
      } %>%
      round(digits)
  }, by = YM]
  tb[, {
    RETURN[is.na(RETURN)] <- 0
    return.serie <- cumprod(RETURN + 1)
    calendar[Y[1], 13] <<- return.serie %>%
      maxdrawdown %>% {
        1 - return.serie[.$TO][1] / return.serie[.$FROM][1]
      } %>% {
        if (percent) {
          (.) %>% multiply_by(100)
        } else {
          .
        }
      } %>%
      round(digits)
  }, by = Y]
  calendar %>%
    as.data.table(keep.rownames=TRUE) %>%
    setnames(1, 'MaxDD(%)')
} # FINISH

balance.mddp.calendar <- function(tickets.edited, tickets.money, percent=TRUE, digits=2) {
  tb <-
    tickets.edited %>%
    rbind(copy(tickets.money) %>% extract(j = NPROFIT := PROFIT), use.names=TRUE, fill=TRUE) %>%
    setkey(CTIME) %>%
    extract(
      j = diff.ratio := {
        bal <- cumsum(NPROFIT)
        NPROFIT / c(0, bal[-length(bal)]) + 1
      }
    ) %>%
    extract(
      i = GROUP != 'MONEY',
      j = .(TIME = CTIME, diff.ratio, YM = format(CTIME, '%Y%m'), Y = format(CTIME, '%Y'))
    )
  years <- year(tb[1, TIME]):year(tb[.N, TIME])
  calendar <- matrix(data = NA, nrow = length(years), ncol = 13,
                     dimnames = list(years, c(strftime(seq.Date(as.Date("2000-01-01"), length.out = 12, by = "months"),
                                                       format = "%b"), 'TOTAL')))
  tb[, {
    year <- substr(YM[1], 1, 4)
    month <- as.numeric(substr(YM[1], 5, 6))
    diff.ratio[is.na(diff.ratio)] <- 0
    return.serie <- cumprod(diff.ratio)
    calendar[year, month] <<-
      return.serie %>%
      maxdrawdown %>% {
        1 - return.serie[.$TO][1] / return.serie[.$FROM][1]
      } %>% {
        if (percent) {
          (.) %>% multiply_by(100)
        } else {
          .
        }
      } %>%
      round(digits)
  }, by = YM]
  tb[, {
    diff.ratio[is.na(diff.ratio)] <- 0
    return.serie <- cumprod(diff.ratio)
    calendar[Y[1], 13] <<- return.serie %>%
      maxdrawdown %>% {
        1 - return.serie[.$TO][1] / return.serie[.$FROM][1]
      } %>% {
        if (percent) {
          (.) %>% multiply_by(100)
        } else {
          .
        }
      } %>%
      round(digits)
  }, by = Y]
  calendar %>%
    as.data.table(keep.rownames=TRUE) %>%
    setnames(1, 'MaxDD(%)')
}

score.yearly.return <- function(yearly.return) {
  if (yearly.return <= 0) {
    return(0)
  }
  if (yearly.return >= 40) {
    return(100)
  }
  yearly.return * 2.5
}
score.mddp <- function(mddp) {
  if (mddp >= 30) {
    return(0)
  }
  100 - mddp * 10 / 3
}
level <- function(score) {
  if (score >= 80) {
    'A'
  } else if (score >= 60) {
    'B'
  } else if (score >= 40) {
    'C'
  } else if (score >= 20) {
    'D'
  } else {
    'E'
  }
}

myAssetsCorImagePlot <- function (x, labels = TRUE, show = c("cor", "test"),
                                  use = c("pearson", "kendall", "spearman"), abbreviate = 3, ...) {
  R = x
  show = match.arg(show)
  use = match.arg(use)
  R = na.omit(R, ...)
  Names = colnames(R) = substring(colnames(R), 1, abbreviate)
  R = as.matrix(R)
  n = NCOL(R)
  if (show == "cor") {
    corr <- cor(R, method = use)
    if (show == "test") {
      test = corr * NA
      for (i in 1:n) for (j in 1:n) test[i, j] = cor.test(R[, 
                                                            i], R[, j], method = use)$p.value
    }
  }
  else if (show == "robust") {
    stop("robust: Not Yet Implemented")
  }
  else if (show == "shrink") {
    stop("robust: Not Yet Implemented")
  }
  corrMatrixcolors <- function(ncolors) {
    k <- round(ncolors/2)
    r <- c(rep(0, k), seq(0, 1, length = k))
    g <- c(rev(seq(0, 1, length = k)), rep(0, k))
    b <- rep(0, 2 * k)
    res <- (rgb(r, g, b))
    res
  }
  ncolors <- 10 * length(unique(as.vector(corr)))
  image(x = 1:n, y = 1:n, z = corr[, n:1], col = corrMatrixcolors(ncolors), 
        axes = FALSE, main = "", xlab = "", ylab = "", ...)
  if (show == "cor") 
    X = t(corr)
  else X = t(test)
  coord = grid2d(1:n, 1:n)
  for (i in 1:(n * n)) {
    text(coord$x[i], coord$y[n * n + 1 - i], round(X[coord$x[i], 
                                                     coord$y[i]], digits = 2), col = "white", cex = 0.7)
  }
  if (labels) {
    axis(2, at = n:1, labels = Names, las = 2, font = 11, pos = 0.5, tick = FALSE, cex.axis = 0.8)
    axis(1, at = 1:n, labels = Names, las = 2, font = 11, pos = 0.5, tick = FALSE, cex.axis = 0.8)
    Names = c(pearson = "Pearson", kendall = "Kendall", spearman = "Spearman")
    if (show == "test") 
      Test = "Test"
    else Test = ""
    title(main = 'Symbols Yiels Correlations')
    # mText = paste("Method:", show)
    # mtext(mText, side = 4, adj = 0, col = "grey", cex = 0.7)
  }
  box()
  invisible()
}
