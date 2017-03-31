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
  choose.files(choice) %>% read.mq.file
}


#### TEST ####
options(warn = 2)
TEST.RESULT.PHASE1 <- test.phase1(7) %T>% print # OK
message('PHASE 1 TEST OK')
TEST.RESULT.PHASE2 <- TEST.RESULT.PHASE1 %>% html.reports.phase2(parallel=FALSE) %T>% print # OK, but parallel in linux not modified
message('PHASE 2 TEST OK')
TEST.RESULT.PHASE3 <- TEST.RESULT.PHASE2 %>% reports.phase3(parallel.tickets=FALSE) %T>% print
message('PHASE 3 TEST OK')
TEST.RESULT.PHASE3[[1]] %>% output.report.html
message('RENDER HTML')

options(warn = 0)
