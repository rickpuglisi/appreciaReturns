#' Check that all tickers are attached and manually attach if necessary
#'
#' @param tickers - a list of tickers
#' @param dataSource - a quantmod dataSource
#' @param dataEnv - the environment where the data is to be attached
#'
#' @return void just attaches the tickers to the environment
#' @export
#'
#' @examples
#' attach_all_tickers(c("AAPL", "MSFT"))
attach_all_tickers <- function(tickers, dataSource="yahoo", dataEnv=sys.frame()) {

  lastWorkDay <- function(d) {
    if(format(d - 1, '%w') %in% c(0, 6))
      # call this function again with d-1
      Recall(d - 1)
    else
      d - 1
  }

  get_lastDate <- function(x) {
    if (exists(x, mode="numeric")) {
      tmp <- get(x, envir=dataEnv, mode='numeric')
      lastDate <- zoo::index(utils::tail(tmp,1))
      sysDate <- lastWorkDay(Sys.Date())
      # TODO: instead of calendar days check for weekend effects
      numDays <- as.numeric(sysDate - lastDate)
      # print(paste(x,"sinceLastDate",numDays))
    } else {
      numDays <- 1000
      # print(paste(x,"does not exist",numDays))
    }
    return(numDays)
  }

  if (dataSource != "yahoo") {
    # override attachSymbols default with another source
    retrieveSymbols <- tickers
    cat("Manually Attaching All Symbols from ", dataSource, "\n")
    retrieved <- quantmod::getSymbols(Symbols = retrieveSymbols, src = dataSource, env = dataEnv, verbose=TRUE)
  } else {
    myCount <- 0
    test_condition <- TRUE
    while (test_condition) {
      # INSTEAD OF TESTING IF EXISTS, WHAT IS THE LAST DATE AND RELOAD IF NOT CURRENT
      numDaysSince = sapply(tickers, get_lastDate)
      # retrieve any ticker whose last date is more than 2 business days in the past
      retrieveSymbols <- tickers[numDaysSince > 2]
      #print("following tickers are already loaded")
      #print(tickers[numDaysSince <= 2])
      if (length(retrieveSymbols) > 0) {
        cat("Manually Attaching Symbols:", retrieveSymbols, " from yahoo\n")
        # split into chunks of 5 and use loop for i in 1:length(chunks) pausing 1 second b/w requests
        chunks = split(retrieveSymbols, ceiling(seq_along(retrieveSymbols)/5))
        for (i in 1:length(chunks)) {
          retrieved <- quantmod::getSymbols(Symbols = chunks[[i]], src = dataSource, env = dataEnv, verbose=TRUE)
          if (i < length(chunks)) {
            Sys.sleep(1)
          }
        }
      }
      test_condition = FALSE
      myCount <- myCount + 1
    }
  }
}

#' Combines Closing Prices from quantmod getSymbols() into one xts object.
#' Assumes tickers are already attached in environment (see attach_all_tickers)
#'
#' @param tickers - a list of tickers to merge
#' @param years - the number of years of data to retrieve
#' @param j - the column number to use from get()
#' @param dataEnv - the environment where the data is attached
#' @param all - a parameter for the merge() function. If true all dates are used. If false all tickers must have date.
#'
#' @return an xts object indexed by Date
#' @export
#'
#' @examples
#' attach_all_tickers(c("AAPL", "MSFT"))
#' merge_historical_prices(c("AAPL", "MSFT"))
merge_historical_prices <- function(tickers, years=5, j=6, dataEnv=sys.frame(), all=FALSE) {

  get_column <- function(ticker) {
    tData <- tryCatch({get(ticker, envir=dataEnv, mode="numeric")[,j]},error=print)
    if (class(tData)[1] != "xts") {
      cat("Attach Symbol Manually.  Failed to get Symbol for ", ticker, "\n")
    }
    return(tData)
  }

  starting_date <- Sys.Date() - as.integer(years*365.25)
  ending_date <- Sys.Date()
  # use a common window of dates
  results <- stats::window(Reduce(merge,lapply(tickers, get_column)), start=starting_date, end=ending_date, all=all)
  # if yahoo, column names will have a .Adjusted after the ticker
  colnames(results) <- gsub(".Adjusted","",colnames(results))
  # if not yahoo, column names will have a .Close after the ticker
  colnames(results) <- gsub(".Close","",colnames(results))
  return(results)
}
