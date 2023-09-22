#' Get option parameters by parsing the option ticker
#'
#' @param s - the ticker symbol of the option
#' @param overrides - a named list of overrides for Usyms
#'
#' @return a list of (usym, putcall(p/c), strike, and timeToExpiry in years)
#' @export
#'
#' @examples
#' get_TickerOptionParameters("BBBY230120C25")
get_TickerOptionParameters <- function(s, overrides=NULL) {

  result = ifelse(financialParsing::is_Option(s), {
    u <- financialParsing::get_Usym(s, overrides)
    pc <- financialParsing::get_PutCall(s)
    k <- financialParsing::get_Strike(s)
    t <- financialParsing::get_TimeToExpiry(s)
    return(list(u=u, pc=pc, k=k, t=t))
  }, {
    u <- s
    pc <- ""
    k <- 0
    t <- 0
    return(list(u=u, pc=pc, k=k, t=t))
  })
  return(result)
}


#' Retrieve and compute option price parameters queried from the Apprecia Access Database
#'
#' @param s - option ticker
#' @param o - option parameters parsed from the ticker
#'
#' @return a list of (optionPrice, usymForwardPrice, dividendAdjustment)
#' @export
#'
#' @examples
#' o = get_TickerOptionParameters("BBBY230120C25")
#' dbo = get_DbOptionParameters("BBBY230120C25", o)
get_DbOptionParameters <- function(s, o) {

  result = ifelse(financialParsing::is_Option(s), {
    optDivds <- appreciaDb::get_AppreciaOptionDivds()
    dbPrices <- appreciaDb::get_AppreciaCurrentPrices()
    d <- optDivds[s,'Expected Divds']
    xd <- as.Date(optDivds[s,'Ex Dividend Date'])
    d <- ifelse(is.na(d) | (as.Date(Sys.Date()) >= xd), 0, d)
    f <- dbPrices[o$u,'Price'] - d
    p <- dbPrices[s,'Price']
    return(list(p=p, f=f, d=d))
  }, {
    p <- 0
    f <- 0
    d <- 0
    return(list(p=p, f=f, d=d))
  })
  return(result)
}


#' GBSVolatility is not vectorized so need to prevent a call when not an option
#'
#' @param p - price of option
#' @param pc - put/call indicator
#' @param S - spot price of underlying security
#' @param X - strike price
#' @param Time - time to expiry in years
#' @param r - interest rate
#' @param b - dividend rate
#'
#' @return the implied volatility as a number
#'
#' @examples
wrap_GBSVolatility <- function(p, pc, S, X, Time, r, b) {
  if (pc != "") {
    fOptions::GBSVolatility(p, pc, S, X, Time, r, b)
  } else {
    0
  }
}

#' GBSGreeks is not vectorized so need to prevent a call when not an option
#'
#' @param name - the name of the greek (e.g. delta) to compute
#' @param pc - put/call indicator
#' @param S - spot price of underlying security
#' @param X - strike price
#' @param Time - time to expiry in years
#' @param r - interest rate
#' @param b - dividend rate
#' @param v - implied volatility
#' @param defaultValue - default value if not an option
#'
#' @return the implied volatility as a number
#'
#' @examples
wrap_GBSGreeks <- function(name, pc, S, X, Time, r, b, v, defaultValue) {
  if (pc != "") {
    fOptions::GBSGreeks(name, pc, S, X, Time, r, b, v)
  } else {
    defaultValue
  }
}

#' GBSOption is not vectorized so need to prevent a call when not an option
#'
#' @param pc - put/call indicator
#' @param p - price (used if not option)
#' @param S - spot price of underlying security
#' @param X - strike price
#' @param Time - time to expiry in years
#' @param r - interest rate
#' @param b - dividend rate
#' @param vol - implied volatility
#'
#' @return the option price as a number
#'
#' @examples
wrap_GBSOption <- function(pc, p, S, X, Time, r, b, vol) {
  if (pc != "") {
    methods::slot(fOptions::GBSOption(pc, S, X, Time, r, b, vol), "price")
  } else {
    p
  }
}


#' Assemble a portfolio's historical prices from Usym prices, proxies, and option models
#'
#' @param tickers a list of tickers to assemble into a portfolio of historical prices
#' @param reference.ticker the reference ticker to run beta regression on when filling in NA values
#' @param override.usyms a named list of overrides for mapping tickers to Usyms
#' @param override.dates a named list of overrides for mapping Usyms to valid dates for filling in NA values
#' @param shiftPct a percent of strike to use for Shift Log-Normal model
#' @param r interest rate to use for option pricing
#' @param years the number of years of historical data to use
#' @param robust a boolean flag specifying whether to compute a robust regression or simple regression
#'
#' @return a xts dataframe of historical prices for a portfolio of tickers
#' @export
#'
#' @examples
#' quantmod::attachSymbols()
#' override.dates = c(QS="2020-09-03", CHRD="2020-11-24")
#' tickers = c("OI221216C22", "QS")
#' tmp = assemble_portfPrices(tickers, reference.ticker="SPY", override.dates = override.dates)
assemble_portfPrices <- function(tickers, reference.ticker="SPY", override.usyms=NULL, override.dates=NULL,
                                 shiftPct = 0.85, r = 0.01, years=5, robust = FALSE) {
  o = get_TickerOptionParameters(tickers, override.usyms)
  dbo = get_DbOptionParameters(tickers, o)
  dsln <- o$k * shiftPct
  # uses AppreciaDb prices for implied vols
  impVol <- mapply(wrap_GBSVolatility, dbo$p, o$pc, dbo$f+dsln, o$k+dsln, o$t, r, 0)
  attach_all_tickers(unique(o$u))
  # using defaults: j=6, dataEnv=sys.frame(), all=FALSE
  uSym.prices <- merge_historical_prices(o$u, years)
  if (!is.null(reference.ticker)) {
    attach_all_tickers(c(reference.ticker))
    uSym.prices <- fill_PricesFromBeta(uSym.prices, reference.ticker, override.dates, robust)
  }
  # must loop over columns because different option parameters for each column
  portfolio.prices = uSym.prices
  for (i in 1:length(tickers)) {
    if (financialParsing::is_Option(tickers[i])) {
      val <- sapply(as.numeric(uSym.prices[,i]),
        function(x) methods::slot(fOptions::GBSOption(o$pc[i],x-dbo$d[i]+dsln[i],o$k[i]+dsln[i],o$t[i],r,0,impVol[i]),
                                  "price"))
      portfolio.prices[,i] <- xts::as.xts(val,order.by=zoo::index(uSym.prices))
    }
  }
  colnames(portfolio.prices) <- tickers
  return(stats::na.omit(portfolio.prices))
}


#' Assemble a portfolio's delta exposures from Usym prices, proxies, and option models
#'
#' @param shares - a dataframe with a Shares column and ticker rownames
#' @param override.usyms a named list of overrides for mapping tickers to Usyms
#' @param shiftPct a percent of strike to use for Shift Log-Normal model
#' @param r interest rate to use for option pricing
#'
#' @return a dataframe of deltas and exposures for a portfolio of tickers
#' @export
#'
#' @examples
assemble_portfExposures <- function(shares, override.usyms=NULL, shiftPct = 0.85, r = 0.01) {
  tickers = rownames(shares)
  o = get_TickerOptionParameters(tickers, override.usyms)
  dbo = get_DbOptionParameters(tickers, o)
  dsln <- o$k * shiftPct
  # parses twice but avoids looping with rbind
  impVol <- mapply(wrap_GBSVolatility, dbo$p, o$pc, dbo$f+dsln, o$k+dsln, o$t, r, 0)
  delta <- mapply(wrap_GBSGreeks, "delta", o$pc, dbo$f+dsln, o$k+dsln, o$t, r, 0, impVol, 1)
  exposure = delta * dbo$f * shares["Shares"]
  colnames(exposure) = "Exposure"
  return(cbind(shares, Delta=delta, exposure))
}


# assemble portfolio's historical prices
# NOW WE ARE TRYING TO GET LOG RETURNS FOR VaR RATHER THAN PRICES
# LOOKS VERY SIMILAR TO GET PRICES EXCEPT THE LAST log(val/opt)
# HERE WE COMPUTE NOT ONLY CURRENT IMPLIED VOL BUT THE PRICE FROM THE IMPLIED VOL TOO
# SO WE ARE ONLY USING MODEL PRICES FOR THE log(val/opt)
#' Title
#'
#' @param tickers a list of tickers to assemble into a portfolio of historical prices
#' @param reference.ticker the reference ticker to run beta regression on when filling in NA values
#' @param override.usyms a named list of overrides for mapping tickers to Usyms
#' @param override.dates a named list of overrides for mapping Usyms to valid dates for filling in NA values
#' @param shiftPct a percent of strike to use for Shift Log-Normal model
#' @param r interest rate to use for option pricing
#' @param years the number of years of historical data to use
#' @param robust a boolean flag specifying whether to compute a robust regression or simple regression
#'
#' @return
#' @export
#'
#' @examples
assemble_VaR_returns <- function(tickers, reference.ticker="SPY", override.usyms=NULL, override.dates=NULL,
                                 shiftPct = 0.85, r = 0.01, years=10, robust = FALSE) {
  o = get_TickerOptionParameters(tickers, override.usyms)
  dbo = get_DbOptionParameters(tickers, o)
  dsln <- o$k * shiftPct
  # parses three times but avoids looping with rbind
  impVol <- mapply(wrap_GBSVolatility, dbo$p, o$pc, dbo$f+dsln, o$k+dsln, o$t, r, 0)
  delta <- mapply(wrap_GBSGreeks, "delta", o$pc, dbo$f+dsln, o$k+dsln, o$t, r, 0, impVol, 1)
  opt <- mapply(wrap_GBSOption, o$pc, dbo$p, dbo$f+dsln, o$k+dsln, o$t, r, 0, impVol)
  # get usym returns
  usym.returns = diff(assemble_portfPrices(unique(o$u), reference.ticker, override.usyms, override.dates,
                                     shiftPct, r, years, robust), log=TRUE, na.pad=FALSE)
  stock.returns <- NULL
  for (i in 1:length(tickers)) {
    print(i)
    usymTicker = o$u[i]
    if (tickers[i] != usymTicker) { #option
      s.adj <- dbo$f[i]*exp(usym.returns[,usymTicker])
      val <- sapply(zoo::coredata(s.adj), function(x) methods::slot(fOptions::GBSOption(o$pc[i],x+dsln[i],o$k[i]+dsln[i],
                                                                    o$t[i],r,0, impVol[i]),"price"))
      val <- xts::as.xts(log(val/opt[i]), order.by=zoo::index(usym.returns))
    } else {
      val <- usym.returns[,usymTicker]
    }
    if (is.null(stock.returns)) {
      stock.returns <- val
    } else {
      stock.returns <- merge(stock.returns, val, all=TRUE)
    }
  }
  colnames(stock.returns) <- tickers
  return(stats::na.omit(stock.returns))
}

