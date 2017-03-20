library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####


#### @PATCH NOTE@ ####
## 2017-02-16: Version 0.1

tickets.supported <- function(tickets.raw, item.symbol.mapping) {
  copy(tickets.raw) %>%
    extract(
      j = SYMBOL := item.symbol.mapping[ITEM[1]], by = ITEM
    ) %>%
    extract(i = GROUP == 'MONEY' | !is.na(SYMBOL))
} # FINISH

unsupported.items <- function(item.symbol.mapping) {
  names(item.symbol.mapping) %>%
    extract(item.symbol.mapping %>% is.na %>% which)
} # FINISH

supported.items <- function(item.symbol.mapping) {
  names(item.symbol.mapping) %>%
    extract(item.symbol.mapping %>% is.na %>% not %>% which)
} # FINISH

item.symbol.mapping <- function(tickets.raw, support.symbols=SUPPORT.SYMBOLS) {
  unique(tickets.raw[GROUP != 'MONEY', ITEM]) %>%
    sapply(item.to.symbol, support.symbols)
} # FINISH