$PROB
- Name: MM001.cpp
- model with parallel linear and Michaelis-Menten elimination with direct-effect inhibitory sigmoid Emax structural model  
- Based on xxxxx dataset
- Author: Feng Yang 
- Date: 2018-11-16  
- NONMEM Run: 12345
- Therapeutic Area:  Oncology
- Structure: two compartment, first order absorption
- Implementation: ODE solutions
- Error model: Additive + proportional
- Covariates:
- WGTBL on clearance, CL
- WGTBL on central colume, V2 
- Random effects on: `CL`, `Q`, `V2`, `V3`

//##############################################################################
// model parameter  code written in $PARAM will be parsed by the R parser                                                            
//##############################################################################

$PARAM @annotated
  TV_CL :  0.12       : Clearance (L/day)
  TV_V2 :  2.60       : Volume of central compartment (L)
  TV_Q  :  0.08       : Inter-compartmental clearance between the central and peripheral compartments (L/day)  
  TV_V3 :  1.20       : Volume of peripheral compartment (L)   
  
  TV_KA :  0.15       : Absorption rate constant (1/day)
  TV_F1 :  0.70       : Bioavailability (unitless)
  
  TV_VMAX  : 5.0      : Maximum elimination clearance in Michael-Menten clerance (mg/day)  
  TV_KSS   : 20       : Concentration at half of VMAX (mg/L)  
   
  TV_EMAX  : 100      : Maximum inhibition (unit of PD response)   
  TV_EC50  : 20       : Concentration at half of EMAX (mg/L)  
  TV_GAMMA : 5.0      : Sigmoidicity shape parameter of the concentration-response relationship (unitless)    

  WGTBL_ON_CLQ : 0.75 : WGTBL as covariate on CL and Q (exponent)
  WGTBL_ON_VSS : 1.00 : WGTBL as covariate on V2 and V3 (exponent)  
  
  WGTBL    : 70       : Baseline body weight   

//##############################################################################
// fixed parameter                                                             
//##############################################################################
$FIXED @annotated
  M_WGTBL : 70  : Mean body weight at baseline, kg
  M_AGE   : 34  : Age, year 
  PD_BL   : 100 : Baseline PD value
  
$SET delta=0.1, end=240         // default setting   


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

double VMAX = exp(log(TV_VMAX) + ETA_VMAX);
double KSS  = exp(log(TV_KSS) + ETA_KSS);

double F1   = TV_F1;    //exp(TV_F1)/(1+exp(TV_F1)); 

CL = CL * exp(WGTBL_ON_CLQ*(log(WGTBL)-log(M_WGTBL))); 
Q  = Q  * exp(WGTBL_ON_CLQ*(log(WGTBL)-log(M_WGTBL)));

V2 = V2 * exp(WGTBL_ON_VSS*(log(WGTBL)-log(M_WGTBL)));
V3 = V3 * exp(WGTBL_ON_VSS*(log(WGTBL)-log(M_WGTBL)));

// derived parameter
_F(1) = F1;
double S2   = V2;                        

double K23 = Q/V2;
double K32 = Q/V3;

double KE  = CL/V2;
double CP  = CENTRAL/V2;
 
double EMAX = TV_EMAX;
double GAMMA  = TV_GAMMA;
double EC50  = TV_EC50;

//##############################################################################
// ODE, Using C++ grammar                                                  
//##############################################################################

$ODE       
  // drug depot
  dxdt_DEPOT  = -KA*DEPOT;   
  // central compartment (amount)
  dxdt_CENTRAL = KA*DEPOT - (KE+K23)*CENTRAL + K32*PERIPH - VMAX*CP/(KSS+CP);  
  // peripheral compartment (amount)
  dxdt_PERIPH  = K23*CENTRAL-K32*PERIPH; 
  
  //INH = IMAX*CP/(IC50 + CP);
  //dxdt_RESP = KIN*(1 - INH) - KOUT*RESP;
 
//##############################################################################
// model parameter (OMEGA, SIGMA)                                                    
//##############################################################################

  $OMEGA @annotated @block @name ETA-CLQ-VSS
  ETA_CLQ: 0.05 : ETA on CLQ
  ETA_VSS: 0.006 0.009 : ETA on VSS 
  
  $OMEGA @annotated @name ETA-KA
  ETA_KA: 0.10 : ETA ON KA
    
  $OMEGA @labels ETA_VMAX  ETA_KSS
  0.05   0  
  
  $OMEGA @labels ETA_EC50  ETA_GAMMA  ETA_EMAX 
  0.2	  1   0.1  
 
  $SIGMA @annotated @name SGMA 
  PROP_PK: 0.05: Proportional residual error
  PROP_PD: 0.01  : Proportional residual error
  

  //##############################################################################
  // TABLE, Using C++ grammar
  //##############################################################################
  
  $TABLE
  double CONC = CENTRAL/V2;     // individual concentration
  double PD = PD_BL - EMAX*pow(CONC,GAMMA)/(pow(EC50,GAMMA)+pow(CONC,GAMMA));   //individual response  
  
  double IPRED_CONC = CONC;     // individual predicted concentration
  double IPRED_PD = PD;        // individual predicted response
    
  double DV_CONC = CONC + CONC*PROP_PK;   //individual predicted concentration (plus residue)
  double DV_PD = PD + PD*PROP_PD;         //individual predicted response (plus residue)

//##############################################################################
// CAPTURE
//############################################################################## 

$CAPTURE @annotated 
  IPRED_CONC  : Individual concentration (mg/L)
  IPRED_PD    : Individual PD response
  
  DV_CONC  : DV for CONC 
  DV_PD    : DV for PD
  
  WGTBL   : Baseline body weight  
  
  CL  : Individual Clearance (L/day)
  V2  : Individual Volume of central compartment (L)
  Q   : Individual Inter-compartmental clearance between the central and peripheral compartments (L/day)  
  V3  : Individual Volume of peripheral compartment (L)   
  KA  : Individual Absorption rate constant (1/day)
  F1  : Individual Bioavailability (unitless)
  
  VMAX  : Individual Maximum elimination clearance in Michael-Menten clerance (mg/day)  
  KSS   : Individual Concentration at half of VMAX (mg/L)  
  
  EC50    : Concentration at half of TVEMAX (mg/L)  
  GAMMA   : Sigmoidicity shape parameter of the concentration-response relationship (unitless)    
  EMAX    : Maximum inhibition (unit of PD response)
    
  WGTBL_ON_CLQ : WGTBL as covariate on CL and Q (exponent)
  WGTBL_ON_VSS  : WGTBL as covariate on V2 and V3 (exponent)  
 
  ETA_CLQ  : Random effect on CL and Q 
  ETA_VSS  : Random effect on volume of distribution (V2 and V3)     
  ETA_KA   : Random effect on KA
  ETA_VMAX : Random effect on VMAX  
  ETA_KSS  : Random effect on KSS
    



