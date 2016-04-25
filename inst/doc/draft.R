## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache = TRUE, echo = FALSE, message = FALSE, warning = FALSE)
library(dplyr)
library(ggplot2)
library(RcppRoll)
library(baker2)

## ------------------------------------------------------------------------
data(monthly)

summary(monthly)

## ------------------------------------------------------------------------
## Note that there are a lot of crazy outlier returns. I need to examine these 
## and deal with them in a sensible fashion. For now, I am just going to get rid
## of them and worry about it later. Just avoiding the non top.1500 seems to 
## almost all the necessary work. But still, there is really no excuse for not 
## looking at the return data much more closely. What about those outliers we 
## saw before?
 
## BE VERY CAREFUL about the na.rm = TRUE in calculating mean return.


z <- monthly %>% group_by(sd.class, date) %>% 
      summarize(ret.0.1.m = mean(ret.0.22.d, na.rm = TRUE)) %>% 
      mutate(cum.ret = cumprod(1 + ret.0.1.m)) %>% 
      ungroup() %>% 
      arrange(sd.class, date)

## Now we have a simple data set that show, for each (month-end) date and 
## sd.class, what the monthly return and cumulative monthly return are. So, we
## plot them.
      
z %>% filter(! is.na(cum.ret)) %>% 
  ggplot(aes(x = date, y = cum.ret, color = sd.class)) + 
    geom_line() + 
    ggtitle("Returns by Volatility Quintile \n January 1999 - December 2007") + 
    xlab("Date") + 
    ylab("Value of $1 Invested in 1999") 

## Why am I getting the opposite answer from Baker? Why is it so hard to make
## this plot look pretty


