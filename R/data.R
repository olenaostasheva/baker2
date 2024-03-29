#'Monthly Data for 1998 -- 2007.
#'
#'A dataset containing monthly data. This is created by first running
#'gather_daily() --- which requires the ws.data package --- and then, on its
#'output, gather_monthly().
#'
#'@format A data frame with 149464 observations on the following 9 variables. 
#'  \describe{ 
#'  \item{\code{symbol}}{a character vector} 
#'  \item{\code{name}}{a character vector} 
#'  \item{\code{date}}{a Date} 
#'  \item{\code{tret}}{a numeric vector} 
#'  \item{\code{cap.usd}}{a numeric vector} 
#'  \item{\code{top.1500}}{a logical vector} 
#'  \item{\code{month}}{a character vector}
#'  \item{\code{year}}{a numeric vector}  
#'  \item{\code{sd.252.0.d}}{a numeric vector} 
#'  \item{\code{ret.0.22.d}}{a numeric vector}
#'   \item{\code{sd.class}}{a factor vector}
#'  }
#'@source Original data source is the ws.data package.
"monthly"