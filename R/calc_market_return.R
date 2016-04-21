#' Calculate the Return of the Market
#' 
#' calc_market_return() takes an input data frame and calculates a measure of
#' the daily return of the market.
#' 
#' @param x a data frame of daily stock return informtion, created by 
#'   \code{clean_data()}.
#'   
#' @return A data frame of daily market returns. 
#'   
#' @import dplyr RcppRoll


calc_market_return <- function(x){
  
  stopifnot(all(c("symbol", "date", "tret", "cap.usd") %in% names(x)))
  
  ## First we need to calculate a market return or each date. Note that any
  ## missing value for the weight variable in a weighted mean causes the entire
  ## calculation to be NA, so we need to remove any such rows.
  
  market.ret <- x %>% group_by(date) %>% 
    filter(! is.na(cap.usd)) %>% 
    summarize(market.ret = mean(tret, w = cap.usd, na.rm = TRUE))
  
  ## Cumulative market returns seem off, with not nearly enough losses in, say,
  ## 2000 and 2001. What is going on?
  
  return(market.ret)
}