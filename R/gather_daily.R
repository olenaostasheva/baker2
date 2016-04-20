#' Gather Data
#' 
#' gather_daily() brings together all the daily return data we need from the 
#' ws.data package. This is not meant to be used interactively, since it
#' requires the ws.data package.
#' 
#' @return A data frame on stock-by-date information.
#'   
#' @import dplyr RcppRoll


gather_daily <- function(){
  
  ## Function for gathering daily data. Not sure if this set up (using "library"
  ## here but not mentioning ws.data in DESCRIPTION) is sensible. I purposely
  ## choose not to export this function because only I should be using it.
  
  ## First, data() the required inputs. 
  
  library(ws.data)
  
  data(daily.1998)
  data(daily.1999)
  data(daily.2000)
  data(daily.2001)
  data(daily.2002)
  data(daily.2003)
  data(daily.2004)
  data(daily.2005)
  data(daily.2006)
  data(daily.2007)
  data(yearly)
  data(secref)
  
  ## Merge in a couple of steps for clarity.
  
  x <- bind_rows(daily.1998, daily.1999, daily.2000, daily.2001,
                 daily.2002, daily.2003, daily.2004, daily.2005,
                 daily.2006, daily.2007)
  

  x <- rename(x, date = v.date) 
  
  ## Need year for the merge with yearly data frame and month (like Jan-2005) to
  ## calculate monthly returns.
  
  x <- mutate(x, year = lubridate::year(date),
                 month = paste(lubridate::month(date, TRUE, TRUE), year, sep = "-"))
  
  x <- left_join(x, select(yearly, -symbol), 
                 by = c("year", "id"))
  
  x <- left_join(x, select(secref, -symbol), by = "id")
  
  x <- select(x, symbol, name, date, tret, top.1500, month) %>% arrange(date, symbol)
  
  ## Calculate key variables for all stocks and all days. Only want to do this
  ## once and then save the data frame in /data.
  
  ## Start with trailing volatility and groupings based on it. Might calculate 
  ## various trailing volatilities? Note that I am not getting exactly one-year
  ## trailing volatility. (That is too hard to worry about now.) Also, I make
  ## the number annualized for easier interpretation.
  
  ## Pay attention to the align left and right arguments! We align right for 
  ## volatility because we are looking at trailing volatility. We align left for
  ## returns because we are looking at future returns.
  
  ## Implicit in the notion of "date" is that, the data in this row is as-of 
  ## 5:00 PM on that date. So, to calculate trailing volatility, I can use the 
  ## data for that date. But, for calculating forward return, I can't. That is
  ## why I need to lead() tret by one day.
  
  ## Some problems are probably caused by calculating the sd.class information 
  ## here, rather than first throwing away all the bad information. Probably not
  ## a big effect, but still.
  
  daily <- x %>% group_by(symbol) %>% 
    mutate(sd.252.0.d = roll_sd(tret, 252, fill = NA, align = "right") * sqrt(252)) %>% 
    mutate(sd.class = ntile(sd.252.0.d, n = 5)) %>% 
    mutate(ret.0.22.d = roll_prod(lead(tret, n = 1)  + 1, 22, fill = NA, align = "left") - 1)
  
  ## Now that I have calculated everything that I care about, I can get rid of 
  ## suspect data. For example, if you are not in the top 1500 companies by 
  ## market cap in your year (meaning as-of January 1), then I don't consider
  ## you investible.
  
  daily <- filter(daily, top.1500)
  
  
  ## I should add some test cases, using test_that. In the meantime, here are a 
  ## couple of calculations that I confirmed by hand. Really ought to check the
  ## return data against something like the S&P.
  
  stopifnot(
    round(subset(daily, date == "2006-07-03" & symbol == "IBM", select = "ret.0.22.d"), 5) == -0.02166,
    round(subset(daily, date == "2007-08-06" & symbol == "GOOG", select = "sd.252.0.d"), 5) == 0.23195
  )

  ## Maybe we don't even need to keep around daily data? Because github is complaing Key 
  ## (incorrect!) simplification above, of course, is that ret.0.22.d and 
  ## monthly returns are not the same thing. Is there a cool way to calculate
  ## what I really want here?
  
  return(daily)
}