
#-------------------------------------------------------------------------------
# individual fitting plot using ggplot
#-------------------------------------------------------------------------------
#' Extracts the time matched concntration vs effect data
#'
#' @param data A data frame.  The data frame must conform to Regeneron standards and contain the variables: `week`, `ndy`, `concn`, `concni`, `pchg`, `actarmcd`, `ada`
#' @return A dataframe with columns `ndy`, `concn`, `effect`.
#' @export
#' @importFrom dplyr left_join group_by summarise
#' @examples
#'   inFile <- system.file("extdata/pk_pd_hysteresis.xls", package = "pkGraph")
#'   library(readxl)
#'   theData <-read_excel(inFile)
#'   timeMatchedPK(data = theData) 
xpdb_diagnostic_GOF3 <- function(xpdb, values4xpdb, n=25, ids=NA, font.size=12)  {
  validate(need(xpdb, message=FALSE))
  
  tdata = slot(xpdb, "Data")   # or xpdb@Data
  runno = slot(xpdb, "Runno")  # or xpdb@Runno  # Plot n individual profiles at random
  
  #set.seed(123)
  x=tdata;   
  
  n = min(n, length(unique(x$ID)))
  if(as.logical(sum(is.na(ids)))) ids = c(sample(unique(x$ID),n)) 
  
  tdata = x[x$ID%in%ids,] %>% mutate(xvar=TIME, yvar=DV)  # brush show the observed data
  
  fig = ggplot(tdata,aes(x=xvar,y=yvar))+ 
    geom_point()+ 
    facet_wrap(~ID)+
    geom_line(aes(x=xvar,y=IPRED, color="Individual Prediction"),lwd=0.7) +
    geom_line(aes(x=xvar,y=PRED, color="Population Prediction"),lwd=0.7) +  
    scale_colour_manual("", 
                        #breaks = c("darkorange", "blue"),
                        values = c("Individual Prediction"="darkorange", 
                                   "Population Prediction"="blue" )
                        ) + 
  
    #geom_vline(data=x[!duplicated(x$ID) &x$ID%in%ids,],aes(xintercept=TTG),alpha=0.5,colour="grey",lwd=1)+
    #theme(legend.position="none") +
    ylab("Observed & model prediction(mg/L)") +   
    theme_bw()+ base_theme(font.size=font.size) + 
    xlab(expression(Time~(days)))
  
  values4xpdb$diagnostic$INDIV_PLOT25$fig = fig 
  values4xpdb$diagnostic$INDIV_PLOT25$data = tdata
  
  return(values4xpdb)
  
}
