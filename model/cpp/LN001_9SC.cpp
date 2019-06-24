$PROB
- Name: LN001.cpp
- Model with linear elimination model 
- Based on xxxx dataset
- Author: Feng Yang
- Client: xxxx 
- Date: 05/18/2019
- NONMEM Run: 12345
- Structure: two compartment, first order absorption
- Implementation: ODE solutions
- Error model: Additive + proportional
- Covariates:
  - WGTBL on clearance (CL) and inter-compartmental clearance (Q)
  - WGTBL on central colume (V2) and peripheral volume (V3)
- Random effects on: `CL`, `Q`, `V2`, `V3` 
 
//##############################################################################
// model parameter  code written in $PARAM will be parsed by the R parser                                                            
//##############################################################################
   
$PARAM @annotated
  TV_CL :  0.173  : Typical clearance (L/day)
  TV_V2 :  3.45  : Typical volume of central compartment (L)
  TV_Q  :  0.530  : Typical inter-compartmental clearance between the central and peripheral compartments (L/day)  
  TV_V3 :  3.01  : Typical volume of peripheral compartment (L)   
  
  TV_KA :  0.232  : Typical absorption rate constant (1/day)
  TV_F1 :  0.835  : Typical bioavailability (unitless)

  TV_VMAX : 0        : Typical maximum elimination clearance in Michael-Menten clerance (mg/day)     
  TV_KSS  : 6.509510 : Typical concentration at half of VMAX (mg/L)     

  WGT_ON_CLQ : 0.75  : WGTBL as covariate on CL/Q (Exponent)
  WGT_ON_VSS : 1.0   : WGTBL as covariate on V2/V3 (Exponent)    
 
  WGTBL   : 75       : Baseline body weight 
  
  COVARIATE : 0      : Run covariate
  SCALING   : 0      : Run covariate
  
//##############################################################################
// fixed parameter                                                             
//##############################################################################
$FIXED @annotated
  M_WGT : 75 : Mean body weight at baseline in the specific population (kg)
  M_AGE : 34 : Mean age in the specific population (year)
  
// delta: step-size in numerical integration
// end: end of simulation period, for example of treatment + followup
$SET delta=0.1, end=240          


//##############################################################################
// model description                                                        
//##############################################################################
$CMT @annotated
 DEPOT    : Extravascular compartment (mg)
 CENTRAL  : Central PK compartment (mg)
 PERIPH   : Peripheral compartment (mg)

//##############################################################################
// derived variable, Using C++ grammar                                                     
//##############################################################################
$MAIN 
  double KA = exp(log(TV_KA) + ETA_KA);
  double CL = exp(log(TV_CL) + ETA_CLQ);
  double V2 = exp(log(TV_V2) + ETA_VSS );
  double V3 = exp(log(TV_V3) + ETA_VSS);
  double Q  = exp(log(TV_Q)  + ETA_CLQ);
  
  double F1   = TV_F1;    //exp(TV_F1)/(1+exp(TV_F1)); 

  if (COVARIATE==1) {
  CL = CL * exp(WGT_ON_CLQ*(log(WGTBL)-log(M_WGT))); //CL*pow(WEIGHTBL/M_WGT, 0.75);  
  Q  = Q  * exp(WGT_ON_CLQ*(log(WGTBL)-log(M_WGT)));
  
  V2 = V2 * exp(WGT_ON_VSS*(log(WGTBL)-log(M_WGT)));
  V3 = V3 * exp(WGT_ON_VSS*(log(WGTBL)-log(M_WGT)));
  }
  
  // derived parameter
  _F(1) = F1;
  double S2   = V2;                        
  
  double K23 = Q/V2;
  double K32 = Q/V3;
  
  double KE  = CL/V2;
  double CP  = CENTRAL/V2;
     
//##############################################################################
// ODE, Using C++ grammar                                                  
//##############################################################################

$ODE       
  // drug depot
  dxdt_DEPOT  = -KA*DEPOT;

  // central compartment (amount)
  dxdt_CENTRAL = KA*DEPOT-(KE+K23)*CENTRAL+K32*PERIPH;  
  
  // peripheral compartment (amount)
  dxdt_PERIPH  = K23*CENTRAL-K32*PERIPH;   


//##############################################################################
// model parameter (OMEGA, SIGMA)                                                    
//##############################################################################

$OMEGA @annotated @name ETA_ON_KA
ETA_KA: 0.174704 : ETA ON KA

$OMEGA @annotated @block @name ETA_CL_VSS
ETA_CLQ: 0.0863948 : ETA on CL
ETA_VSS: 0.0214711 0.0362998 : ETA on VSS

$SIGMA @annotated @name SGMA 
PROP: 0.01  : Proportional residual error
ADD : 0.1   : Additive residual error


//##############################################################################
// TABLE, Using C++ grammar
//##############################################################################

$TABLE 
  int STUDYID = 1111;
  double IPRED = CENTRAL/V2;  
  
  double PROP1=PROP*IPRED;   //  proportional part 
  double ADD1=ADD;    // additive part 
  double SD=sqrt(PROP1*PROP1 + ADD1*ADD1); // Standard deviation using proportional residual error 
       
  double DV = IPRED + SD;
  
  double IPRED_CONC = IPRED;     //IPRED_PD
  double DV_CONC = DV;           //DV_PD
 
//##############################################################################
// CAPTURE (output from simulated result)
//############################################################################## 
$CAPTURE @annotated
  STUDYID: Study ID

  CL : Individual clearance (L/day)
  V2 : Individual volume of central compartment (L)
  Q  : Individual inter-compartmental clearance between the central and peripheral compartments (L/day)  
  V3 : Individual volume of peripheral compartment (L)   
  KA : Individual absorption rate constant (1/day)
  F1 : Individual bioavailability (unitless)
  
  IPRED_CONC  :  Individual plasma concentration (mg/mL)
 
  WGT_ON_CLQ  : WGT as covariate on CL/Q (Exponent)
  WGT_ON_VSS  : WGT as covariate on V2/V3 (Exponent)
  
  ETA_CLQ  : Random effect on CL and Q 
  ETA_VSS  : Random effect on volume of distribution (V2 and V3)     
  ETA_KA   : Random effect on KA
 
 



