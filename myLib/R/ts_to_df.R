#' @title ts_to_df
#' @description convert a ts to a data frame for plotting
#' @param x a ts, zoo or xts object
#' @return data frame with first column the time dimension and second column the values
#' @examples
#' # df<-ts_to_df(hc4)
#' # head(df, 1)
#' ## Jaar hc4
#' ## 1850 0.03
ts_to_df <- function(x){
  z<- data.frame(time(x), as.numeric(x))
  names(z)<-c("Jaar", deparse(substitute(x)) )
  return(z)
}
