
#' Run a regression for each ticker of a portfolio and return a list of betas, correlations, and trackingVols
#'
#' @param portfolio.returns - a data.frame of usym y variables to use in the regression
#' @param reference.returns - a data.frame of the x variable to use in the regression
#' @param robust - a Boolean flag specifying whether to compute a robust regression or simple regression
#'
#' @return a data.frame of beta, correlation, and trackingVol for each usym in portfolio
#' @export
#'
#' @examples
get_betaCalcs <- function(portfolio.returns, reference.returns, robust=FALSE) {

  betas <- array(dim=ncol(portfolio.returns))
  corrs <- array(dim=ncol(portfolio.returns))
  trackingVols <- array(dim=ncol(portfolio.returns))
  for (i in 1:ncol(portfolio.returns)) {
    beta.returns <- stats::na.omit(merge(reference.returns, portfolio.returns[,i]))
    result <- tryCatch(
      {
        if (robust == TRUE) {
          beta.regression <- MASS::rlm(beta.returns[,-1] ~ beta.returns[,1])
        } else {
          beta.regression <- stats::lm(beta.returns[,-1] ~ beta.returns[,1], singular.ok=TRUE)
        }
        b <- beta.regression$coefficients[2]
        names(b)="b"
        trv <- stats::sd(zoo::coredata(beta.regression$residuals)) * sqrt(252)
        c(b, trv=trv)
      },
      error = function(e) {
        c(b=1, trv=0)
      })
    betas[i] <- result["b"]
    trackingVols[i] <- result["trv"]
    corrs[i] <- stats::cor(beta.returns)[1,2]
  }
  beta.calcs <- cbind.data.frame(betas, corrs, trackingVols)
  colnames(beta.calcs) <- c("beta", "correlation", "trackingVols")
  rownames(beta.calcs) <- colnames(portfolio.returns)
  return(beta.calcs)
}


#' Fill all NA price values before a valid date with beta-implied historical prices
#'
#' @param portfolio.prices - a data.frame of historical prices for a portfolio of tickers with NA values
#' @param reference.ticker - the ticker to use in the beta regression when filling in the NA values
#' @param override.dates - a named list of override dates for mapping Usyms to valid dates when filling in NA values
#' @param robust - a boolean flag specifying whether to compute a robust regression or simple regression
#'
#' @return a xts data.frame of historical prices for a portfolio of tickers
#' @export
#'
#' @examples
fill_PricesFromBeta <- function(portfolio.prices, reference.ticker="SPY", override.dates=NULL, robust=FALSE) {

  reference.prices <- get(reference.ticker, mode="numeric")[,6]
  # only use reference rows with matching index
  reference.prices <- reference.prices[zoo::index(portfolio.prices),]
  reference.returns <- diff(reference.prices,log=TRUE, na.pad=FALSE)
  portfolio.returns <- diff(portfolio.prices,log=TRUE, na.pad=FALSE)
  betas <- get_betaCalcs(portfolio.returns, reference.returns, robust)
  print(betas)
  tickers = colnames(portfolio.prices)
  last.prices <- NULL
  last.date <- rep(utils::tail(zoo::index(portfolio.prices),1), ncol(portfolio.prices))
  for (i in 1:ncol(portfolio.prices)) {
    # get the first date (farthest back in time) with a valid price for this usym
    # this date will be used to determine the conversion ratio with reference.ticker
    last.valid <- utils::head(stats::na.omit(portfolio.prices[,i]),1)
    last.date[i] <- zoo::index(last.valid)
    if (!is.null(override.dates)) {
      if(!is.na(override.dates[tickers[i]])) {
        override.date = zoo::as.Date(override.dates[tickers[i]])
        if (override.date > last.date[i]) {
          last.date[i] = override.date
        }
      }
    }
    # print(paste("i = ", i, "ticker = ", tickers[i], " last.date = ", last.date[i], "price = ", portfolio.prices[last.date[i],i]))
    last.prices[i] <- as.numeric(portfolio.prices[last.date[i],i])
    refDenom <- as.numeric(reference.prices[last.date[i],])
    # fill in all NA values before the valid date (other NA values will remain)
    fillRows <- zoo::index(portfolio.prices) < last.date[i]
    if(sum(fillRows) > 0) {
      # compute the replacement values to use for all of the rows
      fillValues <- last.prices[i] * ((as.numeric(reference.prices)/refDenom)^betas[i,1])
      portfolio.prices[fillRows,i] <- fillValues[fillRows]
    }
  }
  return(portfolio.prices)
}
