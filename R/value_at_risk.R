#library(scales)
#library(PerformanceAnalytics)
#library(lattice)


#' Title
#'
#' @param portfolio a dataframe of the portfolio (with a MTM column)
#' @param stock.returns a dataframe of the historical VaR pnls for individual stocks in the portfolio
#'
#' @return
#' @export
#'
#' @examples
computeHistoricalVaRPnls <- function(portfolio, stock.returns) {
  # no cash included
  portfolio.mtm <- sum(portfolio[,"MTM"])
  portfolio.hypMTMs <- xts::as.xts(exp(stock.returns) %*% portfolio[,'MTM'],
                                  order.by=zoo::index(stock.returns))
  portfolio.pnls <- portfolio.hypMTMs - portfolio.mtm
  colnames(portfolio.pnls) <- "VaRPnls"
  return(portfolio.pnls)
}

#' Title
#'
#' @param portfolio.returns the VaR returns of the overall portfolio
#' @param pct the VaR percentage to compute
#'
#' @return
#' @export
#'
#' @examples
get_HistoricalVaR <- function(portfolio.returns, pct) {

  portfolio.VaR <- PerformanceAnalytics::VaR(portfolio.returns, p=pct, method="historical", portfolio_method="single")
  portfolio.VaR <- exp(portfolio.VaR)-1.0
  return(portfolio.VaR)
}

#' Create function for max of (stable, empirical)
#'
#' @param p tbd
#' @param a tbd
#' @param b tbd
#' @param s tbd
#' @param x tbd
#'
#' @return
#' @export
#'
#' @examples
qmax_stable_empirical <- function(p, a, b, s, x) {
  empirical_z <- x[as.integer(length(x)*p),]
  stable_z <- stabledist::qstable(p, a, b) * s
  if (p > 0.5) {
    result <- max(empirical_z, stable_z)
  } else {
    result <- min(empirical_z, stable_z)
  }
  return(result)
}

#' Use n most deviate points to fit a QQ plot to a stable distribution
#'
#' @param qq.theta a list of three parameters of the stable distribution, e.g. c(s = 0.72, alpha = 1.91, beta=-.27)
#' @param x the data to be fitted
#' @param n the number of points to fit
#' @param b a Boolean to determine what to output
#'
#' @return
#' @export
#'
#' @examples
QQ.objective <- function(qq.theta, x, n, b) {
  print(sprintf("s = %f  alpha = %f  beta = %f", qq.theta["s"], qq.theta["alpha"], qq.theta["beta"]))
  x.841 <- x[as.integer(length(x)*0.841)]
  x.159 <- x[as.integer(length(x)*0.159)]
  s <- qq.theta["s"]
  names(s) <- NULL
  z <- x / s
  pp <- stats::ppoints(z)
  if (2*n > length(x)) { n <- as.integer(length(x)/2.0) }
  # sprintf("n = %i", n)
  y <- utils::head(pp,n)
  y <- c(y, utils::tail(pp,n))
  w <- utils::head(z,n)
  w <- c(w, utils::tail(z,n))
  names(w) <- NULL
  qs <- stabledist::qstable(y, qq.theta["alpha"], qq.theta["beta"])
  r <- stats::prcomp( ~ w + qs )
  slope <- r$rotation[2,1] / r$rotation[1,1]
  intercept <- r$center[2] - slope * r$center[1]
  error <- (slope - 1.0)^2+ 0.01*intercept^2
  if (b==TRUE) {
    qq_output <- error
  } else {
    qq_output <- list(slope=slope, intercept=intercept, scale=s, stable.pts=qs, scaled.pts=w) }
  return(qq_output)
}


# get Parametric VaR

#' Title
#'
#' @param portfolio.VaRpnls the VaR pnls of the overall portfolio
#' @param initial.guess of the three parameters for the stable distribution, e.g. c(s = 0.72, alpha = 1.91, beta=-.27)
#'
#' @return
#' @export
#'
#' @examples
get_ParametricVaR <- function(portfolio.VaRpnls, initial.guess) {

  portfolio.sortPnL <- data.frame(sort(drop(zoo::coredata(portfolio.VaRpnls,fmt=TRUE))))
  colnames(portfolio.sortPnL)[1]="PnL"
  portfolio.sd <- sapply(portfolio.sortPnL, stats::sd)
  portfolio.normalized <- scale(portfolio.sortPnL)
  # QQ.objective is a function
  qq.fit <- stats::optim(initial.guess, QQ.objective, x=portfolio.normalized[,1], n=50, b=TRUE,
                         lower=c(0.25, 1.0, -1.0), upper=c(1.0, 2.0, 1.0), control=c(maxit=10))
  qq.result <- QQ.objective(qq.theta=qq.fit$par, x=portfolio.normalized[,1], n=50, b=FALSE)
  portfolio.99.downside <- qmax_stable_empirical(0.01, qq.fit$par[2], qq.fit$par[3], qq.fit$par[1],
                                                 portfolio.normalized)
  portfolio.qqVaR <- portfolio.99.downside * portfolio.sd
  return(list(portfolio.qqVaR, qq.fit, qq.result))
}


#' Title
#'
#' @param dfrm the dataframe of VaR Pnls
#' @param portfolio.VaR1 the 1-year VaR
#' @param portfolio.VaR3 the 3-year VaR
#' @param portfolio.VaR5 the 5-year VaR
#' @param portfolio.VaR10 the 10-year VaR
#' @param portfolio.totalMTM the total MTM of the portfolio including cash
#' @param size the number of Pnls to plot
#' @param pct the percentage VaR computed
#'
#' @return
#' @export
#'
#' @examples
buildPlot <- function(dfrm, portfolio.VaR1, portfolio.VaR3, portfolio.VaR5, portfolio.VaR10, portfolio.totalMTM,
                      size, pct) {
  colMin <- function(data) sapply(data, min, na.rm = TRUE)
  ylimit <- floor(colMin(dfrm)[1] * 100) / 100
  #put colour in aes so can have color legend
  hvarPlot <- ggplot2::ggplot(dfrm, ggplot2::aes(x=as.Date(rownames(dfrm)))) +
    ggplot2::geom_line(ggplot2::aes(y=VaRReturns, colour="1"), stat = "identity") +
    ggplot2::geom_line(ggplot2::aes(y=VaR10yr, colour="2"), stat = "identity", size=2) +
    ggplot2::geom_segment(ggplot2::aes(y=VaR5yr[1], yend=VaR5yr[1],
                     x=as.Date(rownames(dfrm)[(size-5*252)]),
                     xend=as.Date(utils::tail(rownames(dfrm),1)), colour="3"), stat = "identity", size=2) +
    ggplot2::geom_segment(ggplot2::aes(y=VaR3yr[1], yend=VaR3yr[1],
                     x=as.Date(rownames(dfrm)[(size-3*252)]),
                     xend=as.Date(utils::tail(rownames(dfrm),1)), colour="4"), stat = "identity", size=2) +
    ggplot2::geom_segment(ggplot2::aes(y=VaR1yr[1], yend=VaR1yr[1],
                     x=as.Date(rownames(dfrm)[(size-1*252)]),
                     xend=as.Date(utils::tail(rownames(dfrm),1)), colour="5"), stat = "identity", size=2) +
    ggplot2::scale_colour_manual(values=c("blue", "red", "yellow", "orange", "green"),
                        labels=c("Fund", "NyrVaR", "5yrVaR", "3yrVaR", "1yrVaR")) +
    ggplot2::ggtitle(paste('Current Portfolio\'s Daily VaR Returns and ', pct, '% 1-day VaRs', sep='')) +
    ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5), plot.subtitle=ggplot2::element_text(hjust=0.5)) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Percent of AUM") +
    ggplot2::ylim(ylimit, -ylimit) +
    ggplot2::scale_y_continuous(breaks=seq(ylimit,-ylimit,0.01)) +
    ggplot2::guides(colour=ggplot2::guide_legend(title="Data")) +
    ggplot2::annotate("text", x=(Sys.Date() - 0.4*365),
             y=-0.0015+as.numeric(portfolio.VaR1/portfolio.totalMTM),
             label = paste("1yrVaR = ",
                           format(as.numeric(portfolio.VaR1),big.mark=",",digits=0,scientific=FALSE),sep=""),
             size=2.7) +
    ggplot2::annotate("text", x=(Sys.Date() - 2.4*365),
             y=-0.0015+as.numeric(portfolio.VaR3/portfolio.totalMTM),
             label = paste("3yrVaR = ",
                           format(as.numeric(portfolio.VaR3),big.mark=",",digits=0,scientific=FALSE),sep=""),
             size=2.7) +
    ggplot2::annotate("text", x=(Sys.Date() - 4.4*365),
             y=-0.0015+as.numeric(portfolio.VaR5/portfolio.totalMTM),
             label = paste("5yrVaR = ",
                           format(as.numeric(portfolio.VaR5),big.mark=",",digits=0,scientific=FALSE),sep=""),
             size=2.7) +
    ggplot2::annotate("text", x=(Sys.Date() - 7.6*365),
             y=-0.0015+as.numeric(portfolio.VaR10/portfolio.totalMTM),
             label = paste("VaR = ",
                           format(as.numeric(portfolio.VaR10),big.mark=",",digits=0,scientific=FALSE),sep=""),
             size=2.7)
  hvarPlot
  # ggplot2::ggsave(file="portfolioVaR.pdf")
  return(hvarPlot)
}


#' Compute Historical VaRs and plot results
#'
#' @param portfolio a dataframe of the portfolio (with a MTM column)
#' @param stock.returns a dataframe of the historical stock VaRPnl returns
#' @param cash.amount the MTM of cash and debt in portfolio
#' @param pct the percentage VaR to compute
#'
#' @return a plot of the historical VaR and Pnls
#' @export
#'
#' @examples
computePortfolioHistoricalVaRs <- function(portfolio, stock.returns, cash.amount=0, pct=99) {
  portfolio.pnls <- computeHistoricalVaRPnls(portfolio, stock.returns)
  portfolio.totalMTM <- sum(portfolio[,"MTM"]) + cash.amount
  portfolio.returns <- log(1 + portfolio.pnls/portfolio.totalMTM)
  portfolio.VaR10 <- get_HistoricalVaR(portfolio.returns, pct/100) * portfolio.totalMTM
  portfolio.VaR5 <- get_HistoricalVaR(utils::tail(portfolio.returns, 1260), pct/100) * portfolio.totalMTM
  portfolio.VaR3 <- get_HistoricalVaR(utils::tail(portfolio.returns, 756), pct/100) * portfolio.totalMTM
  portfolio.VaR1 <- get_HistoricalVaR(utils::tail(portfolio.returns, 252), pct/100) * portfolio.totalMTM
  sprintf("10 yr Var = %f as percent of MTM = %f", portfolio.VaR10,
          -portfolio.VaR10*100/portfolio.totalMTM)
  sprintf("5 yr Var = %f as percent of MTM = %f", portfolio.VaR5,
          -portfolio.VaR5*100/portfolio.totalMTM)
  sprintf("3 yr Var = %f as percent of MTM = %f", portfolio.VaR3,
          -portfolio.VaR3*100/portfolio.totalMTM)
  sprintf("1 yr Var = %f as percent of MTM = %f", portfolio.VaR1,
          -portfolio.VaR1*100/portfolio.totalMTM)
  size <- nrow(portfolio.pnls)
  #size <- 1260
  dfrm <- cbind(as.data.frame(utils::tail(portfolio.returns, size)),
                (portfolio.VaR10/portfolio.totalMTM),
                (portfolio.VaR5/portfolio.totalMTM),
                (portfolio.VaR3/portfolio.totalMTM),
                (portfolio.VaR1/portfolio.totalMTM))
  colnames(dfrm) <- c("VaRReturns", "VaR10yr","VaR5yr", "VaR3yr", "VaR1yr")
  hvarPlot <- buildPlot(dfrm, portfolio.VaR1, portfolio.VaR3, portfolio.VaR5, portfolio.VaR10,
                        portfolio.totalMTM, size, pct)
  return(hvarPlot)
}

#' Compute Parametric VaR and plot stable distribution QQ results
#'
#' @param portfolio a dataframe of the portfolio (with a MTM column)
#' @param stock.returns a dataframe of the historical stock VaRPnl returns
#' @param cash.amount the MTM of cash and debt in portfolio
#' @param initial.guess the 3 parameters for the stable distribution e.g. c(s = 0.72, alpha = 1.91, beta=-.27)
#'
#' @return
#' @export
#'
#' @examples
computePortfolioParametricVaR <- function(portfolio, stock.returns, cash.amount=0,
                                          initial.guess = c(s = 0.72, alpha=1.91, beta=-0.27)) {
  portfolio.pnls <- computeHistoricalVaRPnls(portfolio, stock.returns)
  portfolio.totalMTM <- sum(portfolio[,"MTM"]) + cash.amount
  portfolio.returns <- log(1 + portfolio.pnls/portfolio.totalMTM)
  portfolio.qqResults <- get_ParametricVaR(portfolio.returns, initial.guess)
  portfolio.paramVaR <- portfolio.qqResults[[1]]
  portfolio.paramVaR <- (exp(portfolio.qqResults[[1]])-1.0) * portfolio.totalMTM
  names(portfolio.paramVaR) <- "ParametricVaR"
  #display stable distribution parameters
  print(sprintf("Final Stable Distribution Parameters: s = %.4f alpha = %.4f beta = %.4f",
          portfolio.qqResults[[2]]$par[1], portfolio.qqResults[[2]]$par[2], portfolio.qqResults[[2]]$par[3]))
  qq.result <- portfolio.qqResults[[3]]
  plot(x=qq.result[["scaled.pts"]], y=qq.result[["stable.pts"]])
  graphics::abline(qq.result[["intercept"]], qq.result[["slope"]])
  portfolio.paramVaRpct <- portfolio.paramVaR / portfolio.totalMTM * 100
  print(sprintf("10 yr 99%% Parametric VaR = %s and as Percent of MTM = %.2f",
          prettyNum(round(portfolio.paramVaR),big.mark=","), -portfolio.paramVaRpct))
  return(portfolio.paramVaR)
}

