context('Phase 1 File Type')
library(data.table)
test.file.path <- './tests/_TEST_FILES/'

test_that(
  desc = 'return the right file-type', {
    # expect_that(test.file.path %>% paste0('MT4-EA_01.htm') %>% html.file.type, is_equivalent_to('MT4-EA'))
    # test.file.path %>% paste0('MT4-EA_01.htm') %>% read.mq.file %T>% print
    capture.output(test.file.path %>% paste0('MT4-EA_01.htm') %>% read.mq.file, print = TRUE)
  }
)