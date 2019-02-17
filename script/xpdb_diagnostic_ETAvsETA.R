
#-------------------------------------------------------------------------------
# ETAvsETA:   ETA correlation
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
xpdb_diagnostic_ETAvsETA <- function(xpdb, values4xpdb, eta_name_lst = paste("ETA", 1:3, sep=""))  {
  validate(need(xpdb, message=FALSE))
  
  tdata = slot(xpdb, "Data")   # or xpdb@Data
  runno = slot(xpdb, "Runno")  # or xpdb@Runno 
  
  ETA=t(combn(eta_name_lst, m=2))
  ETA.lst = paste(ETA[, 1], ETA[, 2], sep="-")
     
  tdata = tdata %>% select(ID, starts_with("ETA")) %>% distinct(ID, .keep_all=TRUE)
  
  #for (i in 1:nrow(ETA)) {
  
  tt = lapply(ETA.lst, function(ETA, tdata) {
    tt = unlist(strsplit(ETA, "-"))
    t1 = tt[1]
    t2 = tt[2]
    
    tdata$ETA_1 = tdata[, t1]
    tdata$ETA_2 = tdata[, t2]
    
    ggplot(tdata, aes(x = ETA_1, y = ETA_2)) +
      #title(paste(t1, " vs. ", t2, sep="")) + 
      geom_point() + #+ stat_smooth(se = F) + 
      stat_smooth(method = "loess", color = "blue", se = F, size = 1.3) + 
      #geom_rug(col=rgb(.5,0,0,alpha=.2)) + 
      base_theme() + 
      xlab(t1) + ylab(t2)
  }, tdata)  
  
  values4xpdb$diagnostic$ETAvsETA = tt
     # multiplot(plotlist=tt, cols=2) 
  return(values4xpdb)
}
