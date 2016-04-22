#' Gather Daily Data
#' 
#' gather_daily() brings together all the daily return data we need from the 
#' ws.data package. This is not meant to be used interactively, since it
#' requires the ws.data package.
#' 
#' @return A data frame on stock-by-date information.
#' 
#' @export
#'   
#' @import dplyr RcppRoll


gather_daily <- function(){
  
  x <- clean_data()
  
  ## Now get rid of the rows that we don't consider investible.
  
  x <- filter(x, top.1500 & ! is.na(sd.252.0.d))
  
  ## Create sd.class. Should probably do beta.class here as well.
  
  daily <- x %>% group_by(date) %>% 
    mutate(sd.class = as.character(ntile(sd.252.0.d, n = 5))) %>%
    mutate(sd.class = ifelse(sd.class == "1", "Low", sd.class)) %>% 
    mutate(sd.class = ifelse(sd.class == "5", "High", sd.class)) %>% 
    mutate(sd.class = factor(sd.class, levels = c("Low", "2", "3", "4", "High"))) %>% 
    ungroup()
  
  ## Double check the data with this plot. There are some outliers, but I am not
  ## sure they matter especially since we only use sd.class in the analysis.
  
  ## ggplot(data = daily, aes(sd.class, log(sd.252.0.d))) + geom_violin() + facet_wrap(~ year)
  

  return(daily)
}