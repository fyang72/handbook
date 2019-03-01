

assign_Q <- function(df, value="DVOR", quartile=c(0, 0.25, 0.5, 0.75, 1), breaks=NULL, 
                     include.lowest = TRUE, right = TRUE, ordered_result = FALSE) {
  
  df = as.data.frame(df)
  DVOR = df[, value]
  
  # using quartile
  if (!is.null(quartile) & is.null(breaks)) {
    breaks=quantile(DVOR, probs=quartile, type=2, na.rm=TRUE)
    #breaks = breaks[2:(length(breaks)-1)]   #############################
    
    #breaks = breaks + seq_along(breaks) * 10^(round(log10(  .Machine$double.eps), digits=0))
    
  }
  stopifnot(!is.null(quartile) | !is.null(breaks))   # one of them must be not NULL
  if(is.null(quartile) & is.null(breaks))   {return(df)}
  
  
  #breaks = sort(unique(c(-Inf,breaks, Inf) ))  # commented out from 0922_2017
  CUT = cut(DVOR, breaks, include.lowest = include.lowest, right = right, ordered_result = ordered_result) 
  
  df$Q_LABEL = CUT
  INT = as.integer(as.factor(CUT))
  MAX_INT = max(INT, na.rm=TRUE)################################
  if (MAX_INT<10) {df$Quantile = paste0("Q", INT)}
  if (MAX_INT>=10) {df$Quantile = paste0("Q", add_prefix(INT, digits=2))}
  if (MAX_INT>=100) {df$Quantile = paste0("Q", add_prefix(INT, digits=3))}
  df
}

