# library(compiler)
# library(R6)
# compilePKGS(T)
# 
# 
# #### @UPDATE IDEA@ ####
# ## 2017-02-22: 
# 
# #### @PATCH NOTE@ ####
# ## 2017-02-22: Version 0.1

library(RMitekeLab)
library(R6)
library(rmarkdown)
library(ggplot2)
library(grid)

MQ_ANALYSTIC <- R6Class(
  classname = 'MetaQuote Analystic',
  public = list(
    #### ACTIONS ####
    
    output.tickets = function(index, tickets='TICKETS.EDITING', groups=c('MONEY', 'CLOSED', 'OPEN'), columns=c(), file.name) {
      if (missing(index)) {
        index <- self$get('selected.index')
      } else {
        index <- self$set('selected.index', index)
      }
      if (!length(index)) {
        return(NULL)
      }
      if (length(index) == 1) {
        report <- self$get.report(index = index)[[1]]
        if (report$PHASE == 1) {
          report <- self$set.report(index = index, value = self$phase2(list(report), index))[[1]]
        }
        # if (report$PHASE == 2) {
        #   report <- self$set.report(index = index, value = self$phase3(list(report)))[[1]]
        # }
      } else {
        report <- self$get('merged.report')
        if (is.null(report) || !identical(report$INDEX, index)) {
          report <- self$set('merged.report', private$build.merged.report())
        }
        # if (report$PHASE == 2) {
        #   report <- self$set('merged.report', value = self$phase3(list(report))[[1]])
        # }
      }
      if (missing(file.name)) {
        file.name <- output.file.name(report$INFOS, type='TICKETS') %>% paste0('.csv')
      }
      # if (grepl('MT4M', report$INFOS[, TYPE])) {
      #   tickets <- 'TICKETS.RAW'
      # }
      output.tickets(tickets=report[[tickets]], groups, columns, file.name)
    },
    
    output.report = function(index, file.name, markdown=private$MARKDOWNS[1]) {

      if (missing(index)) {
        index <- self$get('selected.index')
      } else {
        index <- self$set('selected.index', index)
      }
      if (!length(index)) {
        return(NULL)
      }
      if (length(index) == 1) {
        report <- self$get.report(index = index)[[1]]
        if (report$PHASE == 1) {
          report <- self$set.report(index = index, value = self$phase2(list(report), index))[[1]]
        }
        if (report$PHASE == 2) {
          report <- self$set.report(index = index, value = self$phase3(list(report)))[[1]]
        }
      } else {
        report <- self$get('merged.report')
        if (is.null(report) || !identical(report$INDEX, index)) {
          report <- self$set('merged.report', private$build.merged.report())
        }
        if (report$PHASE == 2) {
          report <- self$set('merged.report', value = self$phase3(list(report))[[1]])
        }
      }
      if (missing(file.name)) {
        file.name <- output.file.name(report$INFOS, type='REPORT') %>% paste0('.html')
      }
      tempReport <- file.path(tempdir(), basename(markdown))
      file.copy(markdown, tempReport, overwrite = TRUE)
      output.report(report, file.name, tempReport, file.type='HTML')
    },
    
    #### PHASES ####
    phase2 = function(report.phase1, index) {
      reports.PHASE2(report.phase1, index,
                     private$DEFAULT.CURRENCY, private$DEFAULT.LEVERAGE, private$DB.OPEN.FUN,
                     private$MYSQL.SETTING, private$TIMEFRAME.TICKVALUE, private$SYMBOLS.SETTING,
                     private$PARALLEL.THRESHOLD.GENERATE.TICKETS)
    },
    phase3 = function(report.phase2, set.init.money=NULL, include.middle=TRUE) {
      reports.PHASE3(report.phase2,
                     set.init.money, include.middle, private$DEFAULT.INIT.MONEY,
                     private$DEFAULT.CURRENCY, private$DB.OPEN.FUN, private$TIMEFRAME.TICKVALUE,
                     private$DB.OHLC.FUN, private$TIMEFRAME.REPORT, private$PARALLEL.THRESHOLD.DB.SYMBOLS,
                     private$MARGIN.BASE, private$SYMBOLS.SETTING, private$PARALLEL.THRESHOLD.GENERATE.TICKETS,
                     private$MYSQL.SETTING)
    },
    
    #### GETTER & SETTER ####
    
    
  ),
  private = list(
    

    MARKDOWNS = c('../markdown/output.report.Rmd'),

    
    
    build.merged.report = function(index=private$selected.index,
                                   default.currency=private$DEFAULT.CURRENCY,
                                   default.leverage=private$DEFAULT.LEVERAGE,
                                   get.open.fun=private$DB.OPEN.FUN,
                                   mysql.setting=private$MYSQL.SETTING,
                                   timeframe=private$TIMEFRAME.TICKVALUE,
                                   symbols.setting=private$SYMBOLS.SETTING,
                                   parallel=private$PARALLEL.THRESHOLD.GENERATE.TICKETS) {
      if (length(index) < 2) {
        return(NULL)
      }
      within(list(), {
        INFOS <- self$get.report('INFOS', index) %>% rbindlist(use.names=TRUE, fill=TRUE)
        CURRENCY <- report.currency(INFOS, default.currency)
        LEVERAGE <- report.leverage(INFOS, default.leverage)
        TICKETS.RAW <- self$get.report('TICKETS.RAW', index) %>% rbindlist(use.names=TRUE, fill=TRUE)
        ITEM.SYMBOL.MAPPING <- item.symbol.mapping(TICKETS.RAW, symbols.setting[, SYMBOL])
        SUPPORTED.ITEM <- supported.items(ITEM.SYMBOL.MAPPING)
        UNSUPPORTED.ITEM <- unsupported.items(ITEM.SYMBOL.MAPPING)
        TICKETS.SUPPORTED <- tickets.supported(TICKETS.RAW, ITEM.SYMBOL.MAPPING)
        TICKETS.EDITING <- tickets.editing(TICKETS.SUPPORTED)
        INDEX <- index
        PHASE <- 2
      })
    },
  )
)
