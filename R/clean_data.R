#' Clean Daily Data
#' 
#' clean_data() brings together all the daily return data we need from the 
#' ws.data package. This is not meant to be used interactively, since it
#' requires the ws.data package.
#' 
#' @return A data frame on stock-by-date information.
#'
#' @export
#'         
#' @import dplyr RcppRoll


clean_data <- function(){
  
  ## Function for cleaning daily data. First, data() the required inputs. Note
  ## the trick of specifying the package.
  
  data(daily.1998, package = "ws.data")
  data(daily.1999, package = "ws.data")
  data(daily.2000, package = "ws.data")
  data(daily.2001, package = "ws.data")
  data(daily.2002, package = "ws.data")
  data(daily.2003, package = "ws.data")
  data(daily.2004, package = "ws.data")
  data(daily.2005, package = "ws.data")
  data(daily.2006, package = "ws.data")
  data(daily.2007, package = "ws.data")
  data(yearly, package = "ws.data")
  data(secref, package = "ws.data")
  
  ## Merge in a couple of steps for clarity. Rename v.date because it is so
  ## aesthetically offensive.
  
  x <- bind_rows(daily.1998, daily.1999, daily.2000, daily.2001,
                 daily.2002, daily.2003, daily.2004, daily.2005,
                 daily.2006, daily.2007)
  
  x <- rename(x, date = v.date) 
  
  ## Need year for the merge with yearly data frame and month to help figue out
  ## the last day in each month later. Maybe I shouldn't bother with that now?
  
  x <- mutate(x, year = lubridate::year(date),
                 month = paste(lubridate::month(date, TRUE, TRUE), year, sep = "-"))
  
  x <- left_join(x, select(yearly, -symbol), 
                 by = c("year", "id"))
  
  x <- left_join(x, select(secref, -symbol), by = "id")
  
  x <- select(x, symbol, name, date, tret, top.1500, month, year, cap.usd) %>% arrange(date, symbol)
  
  ## All these symbols have at least one day in which their total return is 
  ## greater than 700%. I doubt that, so let's just delete those symbols 
  ## completely. Hard to know if this is the best way to do things.
  
  x <- filter(x, ! symbol %in% c("3STTCE", "CHTM", "LDIG", "SCAI", 
                                 "3MFNF", "3CBHDE", "KCI"))
  
  ## Calculate key variables for all stocks and all days. 
  
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
  
  ## Need to think harder about what to do with missing values. Should I remove 
  ## them? For now, I just let a single NA make everything NA and then, in the 
  ## case of sd.252.0.d, I filter you out. Adding ungroup() at the end is an
  ## incantation of sorts.
  
  x <- x %>% group_by(symbol) %>% 
    mutate(sd.252.0.d = roll_sd(tret, 252, fill = NA, align = "right") * sqrt(252)) %>% 
    mutate(ret.0.22.d = roll_prod(lead(tret, n = 1)  + 1, 22, fill = NA, align = "left") - 1) %>% 
    ungroup()
  
  ## I should add some test cases, using test_that. In the meantime, here are a 
  ## couple of calculations that I confirmed by hand. Really ought to check the
  ## return data against something like the S&P.
  
  stopifnot(
    round(subset(x, date == "2006-07-03" & symbol == "IBM", select = "ret.0.22.d"), 5) == -0.02166,
    round(subset(x, date == "2007-08-06" & symbol == "GOOG", select = "sd.252.0.d"), 5) == 0.23195
  )
  
  ## Basic check that the return data frame is of approximately the right size.
  
  stopifnot(nrow(x) > 5000000)
  
  ## Resulting data frame is every stock-date that we could possibly care about.
  ## Only later do we start deleting rows based on NAs and whatnot.
  
  
  return(x)
}