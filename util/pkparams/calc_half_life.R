

# 
# TVCL :  0.125676   : Clearance (L/day)
# TVV2 :  2.67038    : Volume of central compartment (L)
# TVQ  :  0.0753611  : Inter-compartmental clearance between the central and peripheral compartments (L/day)  
# TVV3 :  1.12665    : Volume of peripheral compartment (L)   
# 
# TVKA :  0.155253   : Absorption rate constant (1/day)
# TVF1 :  0.647736   : Bioavailability (unitless)

############################################################################## 
# calculate half life
############################################################################## 
calc.half.life <- function(THETA) {
  # alpha and beta
  parms=NULL
  parms$KE  = THETA[['TVCL']]/THETA[['TVV2']]    # 1/day, 	Elimination rate constant     0.365
  parms$K32 = THETA[['TVQ']]/THETA[['TVV3']];  
  parms$K23 = THETA[['TVQ']]/THETA[['TVV2']]; 
  
  parms=NULL
  parms$KE  = 0.125676/2.67038   # THETA[['TVCL']]/THETA[['TVV2']]    # 1/day, 	Elimination rate constant     0.365
  parms$K32 = 0.0753611 /1.12665  #  THETA[['TVQ']]/THETA[['TVV3']];  
  parms$K23 = 0.0753611/2.67038  #    THETA[['TVQ']]/THETA[['TVV2']]; 
  
  
  alpha.plus.beta  <- parms$KE + parms$K23 + parms$K32
  alpha.multi.beta <- parms$KE * parms$K32
  alpha <- (alpha.plus.beta + sqrt(alpha.plus.beta^2-4*alpha.multi.beta))/2
  beta  <- (alpha.plus.beta - sqrt(alpha.plus.beta^2-4*alpha.multi.beta))/2
  alpha.half.life <- 0.693/alpha
  beta.half.life <- 0.693/beta
  #print(paste("beta.half.life=", beta.half.life, sep=""))
  
  
  
  return(data.frame(alpha.half.life, beta.half.life))
}
