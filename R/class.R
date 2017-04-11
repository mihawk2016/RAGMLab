library(R6)

MetaQuote_Analytics <- R6Class(
  classname = 'MetaQuote Analystics',
  public = list(
    initialize = function() {
      private$SYMBOLS.SETTING <- SYMBOLS.SETTING
      private$MYSQL.SETTING <- MYSQL.SETTING
      private$DB.OPEN.FUN <- DB.O
      private$DB.OHLC.FUN <- DB.OHLC
    },
    add.files = function(files, parallel=private$PARALLEL.THRESHOLD.READ.FILES) {
      files.data <- read.mq.file(files, parallel)
      mismatch.index <-
        sapply(files.data, is.character) %>% which
      if (length(mismatch.index)) {
        private$append('mismatch', unlist(files.data[mismatch.index]))
        if (length(report <- files.data[-mismatch.index])) {
          private$append('report', report)
        }
      } else {
        private$append('report', files.data)
      }
    },
    clear.files = function() {
      private$selected.index = c()
      private$mismatch <- c()
      private$report <- list()
      private$merged.report <- NULL
    },
    
    
    get.report = function(member, index) {
      if (missing(index)) {
        if (length(private$report)) {
          index <- 1:length(private$report)
        } else {
          return(private$report)
        }
      }
      if (missing(member)) {
        return(private$report[index])
      }
      # if (all(member %in% CONTENT.PHASE1)) {
      #   return(private$get.report.simple(member, index))
      # }
      # if (any(member %in% c(CONTENT.PHASE2, CONTENT.PHASE3))) {
      #   null.sub.index <- sapply(private$report[index], function(r) r$PHASE == 1) %>% which
      #   if (length(null.sub.index)) {
      #     null.index <- index[null.sub.index]
      #     self$set.report(index = null.index, value = self$phase2(private$report[null.index], null.index))
      #   }
      # }
      # if (any(member %in% CONTENT.PHASE3)) {
      #   null.sub.index <- sapply(private$report[index], function(r) r$PHASE == 2) %>% which
      #   if (length(null.sub.index)) {
      #     null.index <- index[null.sub.index]
      #     self$set.report(index = null.index, value = self$phase3(private$report[null.index],
      #                                                             set.init.money, include.middle))
      #   }
      # }
      if (length(member) == 1) {
        lapply(private$report[index], function(r) r[[member]])
      } else {
        lapply(private$report[index], function(r) r[member])
      }
    },
    set.report = function(member, index, value) {
      if (missing(index)) {
        index <- 1:length(private$report)
      }
      if (missing(member)) {
        return(private$report[index] <- value)
      }
      private$report[index] <-
        mapply(function(r, m, v) {
          r %>% inset(m, v)
        }, r = private$report[index], v = value, MoreArgs = list(m = member))
    },
    # get.merged.report = function(member) {
    #   if (missing) {
    #     private$merged.report
    #   } else {
    #     private$merged.report[[member]]
    #   }
    # },
    # set.merged.report = function(member, value) {
    #   if (missing) {
    #     private$merged.report
    #   } else {
    #     private$merged.report[[member]] <- value 
    #   }
    # },
    get = function(member) {
      private[[member]]
    },
    set = function(member, value) {
      private[[member]] <- value
    }
  ),
  private = list(
    SYMBOLS.SETTING = NULL,
    PARALLEL.THRESHOLD.READ.FILES = FALSE, # Default Value: 200,
    PARALLEL.THRESHOLD.GENERATE.TICKETS = FALSE, # Default Value: 6,
    PARALLEL.THRESHOLD.DB.SYMBOLS = FALSE, # Default Value: 6,
    DEFAULT.LEVERAGE = 100,
    DEFAULT.CURRENCY = 'USD',
    DEFAULT.INIT.MONEY = 10000,
    MARGIN.BASE = 1500,
    TIMEFRAME.TICKVALUE = 'M1',
    TIMEFRAME.REPORT = 'H1',
    MYSQL.SETTING =NULL,
    DB.OPEN.FUN = NULL,
    DB.OHLC.FUN = NULL,
    
    selected.index = c(),
    mismatch = c(),
    report = list(),
    merged.report = NULL,
    
    
    
    append = function(member, value) {
      private[[member]] %<>% c(value)
    }
  )
)