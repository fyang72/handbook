
$PROB
- based on MM0029_DAT0322
- model with parallel linear and Michaelis-Menten elimination with sigmoid Emax model on Vmax
- Created with 08/03/2018
- Author: Feng Yang
- Client: Regeneron 
- Date: `r Sys.Date()`
- NONMEM Run: 12345
- Structure: two compartment, first order absorption
- Implementation: ODE solutions
- Error model: Additive + proportional
- Covariates:
- WGT on clearance, CLQ
- WGT on central colume, VSS 
- Random effects on: `CL`, `Q`, `V2`, `V3`

//##############################################################################
// model parameter  code written in $PARAM will be parsed by the R parser                                                            
//##############################################################################
$PARAM @annotated
TVCL :  0.556593   : Clearance (L/day)
TVV2 :  5.21342     : Volume of central compartment (L)
TVQ  :  0   : Inter-compartmental clearance between the central and peripheral compartments (L/day)  
TVV3 :  2     : Volume of peripheral compartment (L)   

TVKA :  0.2   : Absorption rate constant (1/day), NOT USED
TVF1 :  0.7   : Bioavailability, NOT USED 

TVVMAX  : 6.08446     : Maximum elimination clearance in Michael-Menten clerance  (mg/day)     
TVKSS   : 2.74282    : Concentration at half of VMAX (mg/L)     

PROP    : 0.26   : Proportional error in PK 
ADD     : 0.01    : Additional error in PK 

WGTBL    : 70      : Baseline body weight   
WGT_ON_CLQ : 0.75      : WGT as covariate on CLQ (Exponent)
WGT_ON_VSS : 1    : WGT as covariate on VSS (Exponent)  

TVEMAX   : -3.93728  : Maximum response in Emax model,   
TVT50    : 15.7303    : Concentration at half of EMAX   
HILL     : 1.0   : Sigmoidicity shape parameter of the concentration-response relationship    

COVARIATE   : 1     : Want to include covariates?
SCALING     : 0     : Want to scale?

//##############################################################################
// fixed parameter                                                             
//##############################################################################
$FIXED @annotated
m_WGT  : 70 : Mean body weight at baseline (kg), ACTUALLY SHOULD BE 80 KG
m_AGE  : 65 : Age, year


$SET delta=0.1, end=240         


//##############################################################################
// model description                                                        
//##############################################################################
$CMT @annotated
DEPOT    : Dosing compartment (mg)
CENTRAL  : Central PK compartment (mg)
PERIPH   : PERIPH



//##############################################################################
// derived variable, Using C++ grammar                                                     
//##############################################################################
$MAIN 
double CL = exp(log(TVCL) + ECLQ);
double Q  = exp(log(TVQ)  + ECLQ);  
double V2 = exp(log(TVV2) + EVSS );
double V3 = exp(log(TVV3) + EVSS);
double KA = exp(log(TVKA) + EKA);

double VMAX = exp(log(TVVMAX) + EVMAX);
double KSS = exp(log(TVKSS) + EKSS);

double F1   = TVF1;    //exp(TVF1)/(1+exp(TVF1));  

double EMAX = TVEMAX * exp(EEMAX);    
double T50 = TVT50 * exp(ET50);  

if (COVARIATE==1) {
  CL = CL * exp(WGT_ON_CLQ*(log(WGTBL)-log(m_WGT)));
  Q  = Q  * exp(WGT_ON_CLQ*(log(WGTBL)-log(m_WGT)));  
  
  V2 = V2 * exp(WGT_ON_VSS*(log(WGTBL)-log(m_WGT)));
  V3 = V3 * exp(WGT_ON_VSS*(log(WGTBL)-log(m_WGT)));  
}

if (SCALING==1) {
  CL = exp(log(CL) + 0.75*(log(WGTBL)-log(m_WGT)));      //CL*pow(WEIGHTBL/m_WGT, 0.75);  
  V2 = exp(log(V2) + 1.00*(log(WGTBL)-log(m_WGT)));      //V2*pow(WEIGHTBL/m_WGT,1);  
  V3 = exp(log(V3) + 1.00*(log(WGTBL)-log(m_WGT)));      //V3*pow(WEIGHTBL/m_WGT, 1);  
  Q  = exp(log(Q)  + 0.75*(log(WGTBL)-log(m_WGT)));      //Q*pow(WEIGHTBL/m_WGT, 0.75);  
  VMAX = exp(log(VMAX)  + 0.75*(log(WGTBL)-log(m_WGT)));      //Q*pow(VMAX/m_WGT, 0.75);  
}

VMAX = VMAX*exp(EMAX*pow(TIME, HILL)/(pow(T50, HILL)+pow(TIME, HILL)));     //; Sigmoid Emax model

// derived parameter
_F(1) = F1;
double S2   = V2;                        

double K23= Q/V2;
double K32= Q/V3;

double KE  = CL/V2;
double CP = CENTRAL/V2;

double CLNL= VMAX/(KSS+CP);
double CLT = CL+CLNL;     

//##############################################################################
// ODE, Using C++ grammar                                                  
//##############################################################################

$ODE       
  dxdt_DEPOT  = -KA*DEPOT;                                                        
dxdt_CENTRAL = KA*DEPOT-(KE+K23)*CENTRAL+K32*PERIPH-VMAX*CP/(KSS+CP);        
dxdt_PERIPH  = K23*CENTRAL-K32*PERIPH;                      


//##############################################################################
// model parameter (OMEGA, SIGMA)                                                    
//##############################################################################

$OMEGA @annotated @name ETA_KA
  EKA: 0 : ETA ON KA
  
  $OMEGA @annotated @name ETA_CLQ
  ECLQ: 1.46E-01: ETA on CLQ
  
  $OMEGA @annotated @name ETA_VSS  
  EVSS: 0.223823 : ETA on VSS 
  
  $OMEGA @annotated @name ETA_VMAX
  EVMAX: 2.40679 : ETA ON VMAX
  
  $OMEGA @annotated @name ETA_KSS
  EKSS: 0 : ETA ON KSS
  
  $OMEGA @annotated @block @name ETA_EMAX
  EEMAX: 0.708122 : ETA on EMAX
  
  $OMEGA @annotated @block @name ETA_T50
  ET50: 2.02468  : ETA on T50
  
  //$SIGMA @annotated @name SGMA 
  //PROP: 0.246646   : Proportional residual error
  //ADD : 11.6942    : Additive residual error
  
  
  //##############################################################################
  // TABLE, Using C++ grammar
  //##############################################################################
  
  $TABLE
  double TEST = 1979;
//double CONC = CENTRAL/V2;
double IPRED = CENTRAL/V2;    

//SD=sqrt(PROP*PROP+ADD*ADD/CONC**2) ;  log-transformed
double SD=sqrt(ADD*ADD+PROP*PROP*CP*CP) ;    //untransformed scale

double DV = IPRED + SD;

//##############################################################################
// CAPTURE
//############################################################################## 
$CAPTURE  CP  CL CLT  CLNL V2 V3 Q KA F1  VMAX KSS T50 EMAX HILL   TEST WGTBL m_WGT  WGT_ON_CLQ  WGT_ON_VSS    ECLQ EVMAX EKA    
  //$CAPTURE @annotated
  //DV  : Concentration (mg/L)
  //ECL : Random effect on clearance (L/hr)
  //CP  : Plasma concentration (mcg/ml)
  //CL  : Individual clearance (L/hr)
  
  
  
  
  
  