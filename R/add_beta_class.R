#' Add Beta Class to Input Data Frame
#' 
#' add_beta_class() takes an input data frame, calculates a measure of beta, adds that
#' measure to the input, and return the new data frame.
#' 
#' @param x a data frame of daily stock return informtion, created by
#'   \code{gather_daily()}.
#' 
#' @return A data frame on stock-by-date information with a new \code{beta.class} column.
#'   
#' @import dplyr RcppRoll


add_beta_class <- function(x){
  
  stopifnot(all(c("symbol", "date", "tret", "cap.usd") %in% names(x)))
  
  ## First we need to calculate a market return or each date.
  
  
  ## Second, use the market return to calculate a covariance for with the market
  ## for each stock on each day over the trailing 252 days.
  
  
  
  
  ## Third, calculate a beta.class variable and then return the new data frame.
  
  

  
  return(x)
}