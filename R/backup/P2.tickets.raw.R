
    TICKETS.RAW <- tickets.raw(infos[, TYPE], file, parse, CURRENCY, get.open.fun, mysql.setting, timeframe, symbols.setting)%>%
      extract(j = c('FILE.INDEX', 'FILE') := list(index, infos[, FILE]))
    
