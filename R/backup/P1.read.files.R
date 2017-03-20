

read.mq.file <- function(mq.files, parallel=PARALLEL.THRESHOLD.READ.FILES) {
  # ''' read mq files (V) '''
  # @param mq.files: MetaQuote files.
  # @return:
  # 2017-02-22: Version 1.0 parallel mode update
  #             @Note: over 15 files goes parallel mode
  # 2017-02-12: Version 0.3 fix file path for input mode
  # 2017-02-07: Version 0.2 parallel
  # 2017-02-05: Version 0.1
  if (is.data.frame(mq.files)) {
    mq.names <- mq.files$name
    mq.files <- mq.files$datapath
  } else {
    mq.names <- basename(mq.files)
  }
  if (is.numeric(parallel)) {
    parallel <- length(mq.files) >= parallel
  }
  if (!parallel) {
    fetched.data <- mapply(fetch.file.data, mq.files, mq.names, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else {
    cluster <- makeCluster(detectCores() - 1)
    fetched.data <- clusterMap(cluster, fetch.file.data, mq.files, mq.names, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    stopCluster(cluster)
  }
  fetched.data
} # FINISH

fetch.file.data <- function(mq.file, mq.file.name) {
  # ''' fetch mq file's data (S) '''
  # @param mq.files: MetaQuote file.
  # @param mq.names: MetaQuote file-name.
  # @return: data of MetaQuote file.
  # 2017-02-05: Version 0.1
  data <-
    if (grepl('.(html|htm)$', mq.file.name)) {
      fetch.html.data(mq.file)
    } else if (grepl('.(xlsx|xls)$', mq.file.name)) {
      ## ToDo ####
      fetch.excel.data(mq.file, mq.file.name)
    } else if (grepl('.(csv)$', mq.file.name)) {
      ## ToDo ####
      fetch.csv.data(mq.file, mq.file.name)
    } else {
      NULL
    }
  if (is.null(data)) {
    mq.file.name
  } else {
    within(data, {
      INFOS %<>%
        extract(j = FILE := mq.file.name)
      PATH <- mq.file
      PHASE <- 1
    })
  }
}

fetch.html.data <- function(mq.file) {
  # ''' fetch mq html's data (S) '''
  # @param mq.files: MetaQuote file.
  # @param mq.names: MetaQuote file-name.
  # @return: data of MetaQuote file.
  # 2017-02-21: Version 0.2 loose coupling for environment
  # 2017-02-05: Version 0.1
  title <-
    tryCatch(
      suppressWarnings(readLines(con8 <- file(mq.file, open = 'rt', encoding = 'UTF-8'), 4, ok = FALSE, warn = FALSE)),
      error = function(e) {
        lines <- readLines(con16 <- file(mq.file, open = 'rt', encoding = 'UTF-16'), 4, ok = FALSE, warn = FALSE)
        close(con16)
        lines
      },
      finally = close(con8)
    ) %>%
    extract((.) %>%
              str_detect('<title>') %>%
              which) %>%
    read_html %>%
    xml_find_first('.//title') %>%
    xml_text
  if (grepl('Strategy Tester:', title)) {
    html.parse <- read_html(mq.file, encoding = 'GBK')
    infos <- fetch.html.data.infos.mt4ea(html.parse)
  } else if (grepl('Statement:', title)) {
    html.parse <- read_html(mq.file, encoding = 'GBK')
    infos <- fetch.html.data.infos.mt4trade(html.parse)
  } else if (grepl('Strategy Tester Report', title)) {
    html.parse <- read_html(mq.file, encoding = 'UTF-16')
    infos <- fetch.html.data.infos.mt5ea(html.parse)
  } else if (grepl('Trade History Report', title)) {
    html.parse <- read_html(mq.file, encoding = 'UTF-16')
    infos <- fetch.html.data.infos.mt5trade(html.parse)
  } else if (grepl('Closed Trades Report', title)) {
    html.parse <- 'NEEDLESS'
    infos <- fetch.html.data.infos.mt4m_closed()
  } else if (grepl('Raw Report', title)) {
    html.parse <- 'NEEDLESS'
    infos <- fetch.html.data.infos.mt4m_raw()
  } else {
    return(NULL)
  }
  list(
    HTML.PARSE = html.parse,
    INFOS = infos
  )
}

