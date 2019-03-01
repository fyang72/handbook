
#--------------------------------------------
# print.std.title
#--------------------------------------------
print.std.title <- function(type="figure" , key ="Mean(SE) ",
                            log.scale.name = "Log-Scaled ",
                            y.name = "Concentrations of Total Evinacumab in Serum ",
                            x.name = "Nominal Sample Time (Day) ",
                            by.name = "Dose Regimen ",
                            patient.name = "Patients with Elevated TG and LDL-C ",
                            dosing.name = "Following Multiple Subcutaneous or Intravenous Dose(s) of Evinacumab ",
                            study.name = "Study R1500-CL-1321"
)   {
  if (type=="figure") versus = "versus "
  if (type=="table") versus = ""
  paste(key, log.scale.name, y.name, versus, x.name, "by ", by.name, "in ", patient.name, dosing.name,  "(", study.name, ")", sep="")
}

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


