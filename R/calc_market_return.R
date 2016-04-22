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
  
  ## Let's doublecheck that we are beginning with, approximately, all the data.
  ## This should be the result of clean_data(), not of gather_daily().
  
  stopifnot(nrow(x) > 5000000)
  
  ## Remove rows that don't belong. If you don't have a cap.usd, you can't
  ## participate in the calculation of market returns. If you are not in the top
  ## 1500 at the start of the year, you are too small to matter.
  
  y <- filter(x, ! is.na(cap.usd)) %>% filter(top.1500) %>% 
    select(symbol, name, date, tret, cap.usd)
  
  stopifnot(nrow(y) < 3600000)

  ## Calculate a market return or each date. Note that any
  ## missing value for the weight variable in a weighted mean causes the entire
  ## calculation to be NA, so we need to remove any such rows.
  
  z <- y %>% group_by(date) %>% 
    summarize(mret = weighted.mean(tret, w = cap.usd, na.rm = TRUE))

  
  ## Merge in the SPX returns
  
  data(spx)
   
  all <- left_join(z, spx, by = "date") %>% select(-id) %>% 
      rename(spx.ret = return) %>%
    mutate(cumret     = cumprod(mret + 1) - 1) %>% 
    mutate(spx.cumret = cumprod(spx.ret + 1) - 1)
      
  
  ## Hard to be sure that we are getting exactly the correct answer here, 
  ## especially since our weights are constant for the whole year. So, later in 
  ## the year, we are giving too much weight to companies that are down. I don't
  ## see why this should bias things but . . .
  
  ## Critically important, it seems, is what market data we are feeding in. In 
  ## particular, we need to be careful about who makes it into the universe and 
  ## who does not. For example, since ws.data includes all stocks that were ever
  ## in the top 1500 --- even during the times when they were too small --- 
  ## using the entire data set is biased. It only contains small companies that 
  ## became big, not the small companies that stayed small. (Maybe this isn't 
  ## too bad because we weight by market cap so small companies can't matter
  ## much.)
  
  ## Nothing obviously wrong with this data. For 2007 (the farthest back that 
  ## quantmod goes in getting SPY), the correlation between SPY and our returns
  ## is almost 96%, with means that are only 3 bps apart.
  
  
  
  ## Cumulative market returns seem off, with not nearly enough losses in, say, 
  ## 2000 and 2001. Consider two specific dates: April 13, 2000 and October 10, 
  ## 2002. The SPY was down -1.3886% on he first an up 3.2394% on the second. 
  ## But we calculate depending on the universe composition, less negative for
  ## the first day and more positive for the second?
  
  ## filter(daily, date %in% as.Date(c("2000-04-13", "2002-10-10")) & !
  ## is.na(cap.usd))  %>% group_by(date)  %>% summarize(y = weighted.mean(tret,
  ## w = cap.usd, na.rm = TRUE))
  
  return(all)
}