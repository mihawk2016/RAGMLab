rm(list = ls())

files.type <- c('MT4-EA', 'MT4-Trade', 'MT5-EA', 'MT5-Trade', 'MT4M-Closed', 'MT4M-Raw')
all.files <- './tests/_TEST_FILES/' %>% paste0(dir('./tests/_TEST_FILES/'))

choose.files <- function(choice) {
  if (missing(choice)) {
    return(all.files)
  }
  if (is.numeric(choice)) {
    return(all.files[choice])
  }
  all.files[str_detect(all.files, choice)]
}


test.phase1 <- function(choice) {
  choose.files(choice) %>% read.mq.file %T>% print
}


#### TEST ####
# TEST.RESULT <- test.phase1('MT4M-Raw') %>% lapply(html.tickets.raw) %T>% print
TEST.RESULT <- test.phase1('MT4-EA') %>% lapply(html.report.phase2) %T>% print