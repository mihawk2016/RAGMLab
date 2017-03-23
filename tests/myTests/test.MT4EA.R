

test.file.path <- './tests/_TEST_FILES/'

file1 <- 'MT4-EA_01.htm' %>% paste0(test.file.path, .) %>% read.mq.file %T>%print
file2 <- 'MT4-EA_02_modify.htm' %>% paste0(test.file.path, .) %>% read.mq.file %T>%print
file3 <- 'MT4-EA_03_partclose.htm' %>% paste0(test.file.path, .) %>% read.mq.file %T>%print
