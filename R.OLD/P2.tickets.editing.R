library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####


#### @PATCH NOTE@ ####
## 2017-02-17: Version 0.1

tickets.editing <- function(tickets.supported, with.open=FALSE) {
  setkey(tickets.supported, GROUP)
  if (!with.open) {
    return(tickets.supported['CLOSED'])
  }
  ## ToDo: recalculate profit etc. with open tickets, then return open $ closed
  
}

# generate.tickets.editing2 <- function(report) {
#   lapply(report, function(r) {
#     within(r, TICKETS.EDITING <- tickets.editing(r$TICKETS.SUPPORTED))
#   })
# }