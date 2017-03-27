library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####

#### @PATCH NOTE@ ####
## 2017-03-26: Version 0.1


tickets.edit.init <- function(report) {
  # report$TICKETS %>% extract(i = GROUP == 'MONEY' | (GROUP %in% c('OPEN', 'CLOSED') & !is.na(SYMBOL)))
  report$TICKETS %>% extract(i = GROUP == 'CLOSED') & !is.na(SYMBOL)
} # FINISH


report.edit.reset <- function(report) {
  if (report$PHASE < 3) {
    return(report)
  }
  within(report, {
    ## TODO delete some Phase 4 element
    
    TICKETS.EDIT <- NULL
    PHASE <- 2
  })
}

report.edit.focus.tickets <- function(report) {
  if (is.null(report$TICKETS.EDIT)) {
    tickets.supported(report) %>%
      setattr('MONEY.INIT', NULL) %>%
      setattr('MONEY.MIDDLE.INCLUDE', TRUE) %>%
      setattr('OPEN', 'INCLUDE') %>% # 'CLOSE' for close the open tickets and join open; 'INCLUDE' for just show;
                                     # 'EXCLUDE' for not show;
      setattr('PENDING.INCLUDE', TRUE) %>%
      setattr('WORKING.INCLUDE', TRUE)
      
  } else {
    report$TICKETS.EDIT
  }
}

#### EDIT GROUP ####
report.edit.money.init <- function(report, mony.init=NULL) {
  tickets <- report.edit.focus.tickets(report)
  within(report, {
    TICKETS.EDIT <- tickets %>% setattr('MONEY.INIT', money.init)
  })
}

report.edit.money.middle <- function(report, include=TRUE) {
  tickets <- report.edit.focus.tickets(report)
  within(report, {
    TICKETS.EDIT <- tickets %>% setattr('MONEY.MIDDLE.INCLUDE', include)
  })
}

report.edit.open <- function(report, open='CLOSE') {
  tickets <- report.edit.focus.tickets(report)
  within(report, {
    TICKETS.EDIT <- tickets %>% setattr('OPEN', open)
  })
}

report.edit.pending <- function(report, include=TRUE) {
  tickets <- report.edit.focus.tickets(report)
  within(report, {
    TICKETS.EDIT <- tickets %>% setattr('PENDING.INCLUDE', include)
  })
}

report.edit.working <- function(report, include=TRUE) {
  tickets <- report.edit.focus.tickets(report)
  within(report, {
    TICKETS.EDIT <- tickets %>% setattr('WORKING.INCLUDE', include)
  })
}

#### EDIT TICKETS ####
# report.tickets.for.edit <- function(report) {
#   tickets <- report.edit.focus.tickets(report)
#   open.mode <- attr(tickets, 'OPEN')
#   if (open.mode == 'CLOSE') {
#     ## TODO
#   } else {
#     tickets
#   }
# }

report.tickets.close.the.open <- function(report) {
  ## TODO
}


