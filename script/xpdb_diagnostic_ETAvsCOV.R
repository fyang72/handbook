



# it is common to add marginal-histograms around a scatter plot
# http://stackoverflow.com/questions/8545035/scatterplot-with-marginal-histograms-in-ggplot2

# add lowess fit


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
#'    
  



xpdb_diagnostic_ETAvsCOV <- function(xpdb, values4xpdb, 
                                     cov_name_lst=c("AGE", "WEIGHTBL", "HEIGHTBL", "BMIBL", "BSABL"), 
                                     eta_name_lst=c("ETA1", "ETA2", "ETA3", "ETA4")) {
  
  validate(need(xpdb, message=FALSE))
  
  tdata = slot(xpdb, "Data")   # or xpdb@Data
  runno = slot(xpdb, "Runno")  # or xpdb@Runno 
  
  
  library(PKPDmisc)
  #library(knitr)
  library(lazyeval)
  library(tidyverse)
  
  #tdata = adpx %>% distinct(USUBJID, .keep_all =TRUE)
  
  # gather all covariates from BW to CRCL to cov_name and cov_value
  #col.lst = c("AGE", "WEIGHTBL", "HEIGHTBL", "BMIBL",    "BSABL")
  g_eta_cov <- tdata %>%  gather(cov_name, cov_value, one_of(cov_name_lst))
  g_eta_cov$cov_value = as_numeric(g_eta_cov$cov_value)
  
  ## Double stack, We can actually gather again
  #col.lst = c("ETA1", "ETA2", "ETA3", "ETA4")
  g2_eta_cov <- g_eta_cov %>% gather(eta_name, eta_value, one_of(eta_name_lst) )
  g2_eta_cov$eta_value = as_numeric(g2_eta_cov$eta_value)
  
  g2_eta_cov = g2_eta_cov %>% mutate(eta_name =ordered(eta_name, levels=eta_name_lst))  
  g2_eta_cov <- g2_eta_cov %>% filter(!is.na(eta_value), !is.na(cov_value))
  #kable(head(g2_eta_cov))
  #kable(tail(g2_eta_cov))
  
  
  eta_cov_scatter0 <- function(df, xval = "cov_value", yval, cov_name = "cov_name") {
    lazy_plot <- lazyeval::interp(~ggplot(distinct(df, cov_value, cov_name, ETA1, eta_name), 
                                          aes(x = cov_value, y = ETA1)) +
                                    geom_point() + #+ stat_smooth(se = F) + 
                                    stat_smooth(method = "loess", color = "blue", se = F, size = 1.3) + 
                                    #geom_rug(col=rgb(.5,0,0,alpha=.2)) + 
                                    base_theme() +
                                    ylab("") +   
                                    facet_wrap(~cov_name, scales="free"),
                                  cov_value = as.name(xval),
                                  ETA1 = as.name(yval),
                                  cov_name = as.name(cov_name))
    return(lazyeval::lazy_eval(lazy_plot))
  }
  
  ### plot all releationships 
  split_eta_cov <- g2_eta_cov %>% split(.$cov_name)  
  bp = lapply(split_eta_cov, function(x) {
    cov_name <- unique(x$cov_name)
    eta_cov_scatter0(x, xval = "cov_value", yval="eta_value", cov_name = "cov_name") +  
      facet_wrap(~eta_name, scales = "free") +
      #ggtitle(cov_name) +
      xlab(cov_name) + 
      theme(legend.position = 'none')   
  })
  
  values4xpdb$diagnostic$ETAvsCOV = bp
  #return(bp)
  
  return(values4xpdb)
}
