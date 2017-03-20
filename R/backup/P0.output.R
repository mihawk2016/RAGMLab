library(compiler)
compilePKGS(T)


#### TICKETS ####
output.tickets <- function(tickets, groups, columns, file.name) {
  copy(tickets) %>%
    setkey(GROUP) %>%
    extract(
      j = c('OTIME', 'CTIME') := list(time.numeric.to.posixct(OTIME), time.numeric.to.posixct(CTIME))
    ) %>%
    extract(
      i = groups,
      j = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
            'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', columns),
      nomatch = 0,
      with = FALSE
    ) %>%
    write.csv(file = file.name)
} # FINISH

#### UTILS ####
output.file.name <- function(infos, type=c('REPORT', 'TICKETS')) {
  ifelse(nrow(infos) == 1, gsub('\\.[^\\..]*$', '', infos[, FILE]), 'FileSet') %>%
    sprintf('%s-%s', substr(type, 1, 1), .)
} # FINISH

