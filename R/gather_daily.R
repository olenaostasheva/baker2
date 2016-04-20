#' Gather Daily Data
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
  
  ## First, data() the required inputs. Not sure how to handle the need for 
  ## library(ws.data) here. For a while, I thought the solution was to have 
  ## requireNamespace(ws.data) here, but that does not work. So, for now, I just
  ## need to remember to use library(ws.data) at the prompt before running this.
  
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
  
  ## Merge in a couple of steps for clarity. Rename v.date because it is so
  ## aesthetically offensive.
  
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
  
  x <- select(x, symbol, name, date, tret, top.1500, month, year) %>% arrange(date, symbol)
  
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
  
  ## Need to think harder about what to do with missing values. Should I remove 
  ## them? For now, I just let a single NA make everything NA and then, in the 
  ## case of sd.252.0.d, I filter you out. Adding ungroup() at the end is an
  ## incantation of sorts.
  
  x <- x %>% group_by(symbol) %>% 
    mutate(sd.252.0.d = roll_sd(tret, 252, fill = NA, align = "right") * sqrt(252)) %>% 
    mutate(ret.0.22.d = roll_prod(lead(tret, n = 1)  + 1, 22, fill = NA, align = "left") - 1) %>% 
    ungroup()
  
  ## Now that I have calculated trailing and leading measures, I can get rid of 
  ## suspect data. For example, if you are not in the top 1500 companies by 
  ## market cap in your year (meaning as-of January 1), then I don't consider 
  ## you investible. Also, if you don't have a trailing volatility, you are
  ## gone.
  
  x <- filter(x, top.1500 & ! is.na(sd.252.0.d))
  
  ## All these symbols have at least one day in which their total return is 
  ## greater than 700%. I doubt that, so let's just delete those symbols 
  ## completely. Note that, now that I am getting rid of NA sd.252.0.d, many of 
  ## these are already gone (or at least the key rows are) but I am leaving this
  ## here on general principals.
  
  x <- filter(x, ! symbol %in% c("3STTCE", "CHTM", "LDIG", "SCAI", 
                                 "3MFNF", "3CBHDE", "KCI"))
  
  
  ## There may be some other outlier returns that I should remove here. For 
  ## example, BRCM has a return of 150% on January 9, 2004 which is not present 
  ## in Bloomberg. But Bloomberg does not have a split either. Fortunately, the
  ## new  getting rid of NA sd.252.0.d removes that BRCM row anyway.
  
  ## Note that, before, I mistakenly included this calculation of sd.class 
  ## above, which was very wrong since that grouping was by symbol. Instead, we 
  ## need to group_by date. Some of the other big moves seem suspect as well,
  ## but it is hard to check.
  
  daily <- x %>% group_by(date) %>% 
    mutate(sd.class = as.character(ntile(sd.252.0.d, n = 5))) %>%
    mutate(sd.class = ifelse(sd.class == "1", "Low", sd.class)) %>% 
    mutate(sd.class = ifelse(sd.class == "5", "High", sd.class)) %>% 
    mutate(sd.class = factor(sd.class, levels = c("Low", "2", "3", "4", "High"))) %>% 
    ungroup()
  
  ## Double check the data with this plot. There are some outliers, but I am not
  ## sure they matter especially since we only use sd.class in the analysis.
  
  ## ggplot(data = daily, aes(sd.class, log(sd.252.0.d))) + geom_violin() + facet_wrap(~ year)
  

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