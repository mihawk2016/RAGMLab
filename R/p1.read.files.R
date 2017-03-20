library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####
## 2017-02-23: MT4-EA/Trade raw tickets code optimize, parse data earlier catch.
## 2017-02-16: @DONE loose coupling for environment
## 2017-02-16: @DONE parallel problem, they should just return values

#### @PATCH NOTE@ ####
## 2017-03-21: NEW VERSION for Ubuntu & Optimize
## 2017-02-23: REPORT list include:
##              PHASE 1: c('INFOS', 'HTML.PARSE', 'PATH') 
##              PHASE 2: c('CURRENCY', 'LEVERAGE', 'TICKETS.RAW', 'ITEM.SYMBOL.MAPPING',
##                         'SUPPORTED.ITEM', 'UNSUPPORTED.ITEM', 'TICKETS.SUPPORTED') 
## 2017-02-22: Version 0.2 loose coupling for environment
## 2017-02-05: Version 0.1




#### FILE TYPES ####
FILE.TYPES <- c('MT4-EA' = 'Strategy Tester:',
                'MT4-TRADE' = 'Statement:',
                'MT5-EA' = 'Strategy Tester Report',
                'MT5-TRADE' = 'Trade History Report',
                'MT4M-CLOSED' = 'Closed Trades Report',
                'MT4M-RAW' = 'Raw Report')

html.file.type <- function(mq.file) {
  tryCatch(
    suppressWarnings(readLines(con8 <- file(mq.file, open = 'rt', encoding = 'UTF-8'), 4, ok = FALSE, warn = FALSE)),
    error = function(e) {
      lines <- readLines(con16 <- file(mq.file, open = 'rt', encoding = 'UTF-16'), 4, ok = FALSE, warn = FALSE)
      close(con16)
      lines
    },
    finally = close(con8)
  ) %>%
    extract((.) %>% str_detect('<title>') %>% which) %>%
    read_html %>%
    xml_find_first('.//title') %>%
    xml_text %>%
    str_detect(FILE.TYPES, .) %T>% print %>%
    which %>%
    names(FILE.TYPES)[.]
}