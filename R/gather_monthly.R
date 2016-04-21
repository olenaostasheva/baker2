#' Gather Monthly Data
#' 
#' gather_monthly() takes a data frame for daily return data and returns a data 
#' frame of monthly data that is stored in data/ and used in the vignette. This 
#' is not meant to be used interactively.
#' 
#' @param x a data frame of daily stock return informtion, created by
#'   \code{gather_daily()}.
#'   
#' @return A data frame on stock-by-date information on a monthly basis.
#'   
#' @import dplyr RcppRoll


gather_monthly <- function(x){
  
  
  
  
  
  ## We really only need this data for the last day of the month, so, we can 
  ## filter out the rest as here. Keep in mind that this is the last trade day
  ## of the month, not always the last day. 
  
  monthly <- x %>% group_by(month) %>% 
    filter(min_rank(desc(date)) == 1 & ! is.na(sd.252.0.d)) 
  
  return(monthly)
}