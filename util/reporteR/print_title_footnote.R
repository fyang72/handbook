 
#--------------------------------------------
# fig.title.std.errbar
#--------------------------------------------
fig.title.std.errbar <- function(log.scale="log", pk.name, drug.name, study.name, route=NULL, patient=NULL, arma=NULL) {
  if (log.scale=="nonlog") log.scale.name = ""
  if (log.scale=="log") log.scale.name = "Log-Scaled"
  
  paste("Mean (SE) ", log.scale.name, " Concentrations of ", pk.name,
        " in Serum vs Nominal Sampling Day Following Multiple Subcutaneous or Intravenous Dose(s) of "
        , drug.name, " (", study.name, ")", sep="")
}



#--------------------------------------------
# fig.footnote
#--------------------------------------------
fig.footnote <- function(log.scale="log", LLOQ=0.078) {
  
  if (log.scale=="nonlog") {footnote = paste("QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous.  Note: Concentrations below the lower limit of quantification (LLOQ) are set to zero.", sep="") }
  
  if (log.scale=="log") {footnote = paste("QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous. Note: Concentrations below the lower limit of quantification (LLOQ, horizontal dotted line = ", LLOQ, " mg/L) are imputed as LLOQ/2 = ", round(LLOQ/2, digits=3), " mg/L.", sep="")
  }
  return(footnote)
  
}
#--------------------------------------------
#  tab.footnote
#--------------------------------------------
tab.footnote <- function() {
  
  footnote = "HR = Hour(s); QW = Once a week; Q2W = Once every two weeks; SC = Subcutaneous; N = Number of patients; SD = Standard deviation; SE = Standard error"
  return(footnote)
  
}


