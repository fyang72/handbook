
$PROB

# LN_BASE_WT.model

- model with parallel linear and Michelis-Menten elimination (concentration only)
- Updated with 03/2017 PK dataset
- Author: Pmetrics Scientist
- Client: Pharmaco, Inc.
- Date: `r Sys.Date()`
- NONMEM Run: 12345
- Structure: two compartment, first order absorption
- Implementation: ODE solutions
- Error model: Additive + proportional
- Covariates:
  - WT on clearance
  - WT on central colume 
- Random effects on: `CL`, `V2`, `Q`
 


//##############################################################################
// model parameter  code written in $PARAM will be parsed by the R parser                                                            
//##############################################################################
$PARAM TVCL=0.28693, TVV2=3.33857, TVQ=0.647261, TVV3=1.68879, TVKA=0.4, TVVMAX=0.0, TVKSS=29.2, TVF1=0.7, RUVCV=0.179543, RUVSD=1.33522, TVEMAX=-0.382415, TVT50=32.0746, HILL=3.16533, WGT_ON_CLQ=0.45423, ALB_ON_CLQ=-1.00381, ALT_ON_CLQ=-0.0817805, IGG_ON_CLQ=0.182303,  WGT_ON_VSS=0.935131, BMI_ON_VSS=-0.552805, BLK_ON_T50=0.946409, WGTBL=75, ALBBL=38, IGGBL=9.7, BMIBL=26.5, ALTBL=21, RACEN=1, COVARIATE=1, SCALING=0
 
 
//$PARAM @annotated
//TVCL :  1.57  : Clearance (L/day)
//TVV2 :  3.49  : Volume of distribution (L)
//TVKA :  0.15  : Absorption rate constant (1/day)
//WT   : 70    : Weight (kg)
//SEX  : 1     : Male = 0, Female 1
//WTCL : 0.75  : Exponent weight on CL
  
//$NMXML
//run = 1005
//project=path
//theta=TRUE, omega=TRUE,sigma=TRUE
//olabels = s(ECL, EVC, EKA)
//slabels = s(PROP, ADD)

//##############################################################################
// fixed parameter                                                             
//##############################################################################
$FIXED
g = 9.8
//M_WGT = 75      //kg
//m_AGE = 62  // year

 M_WGT=75  
 M_AGE=60  
 M_HGTBL=170 
 M_BMIBL=26.5
 M_BSABL=1.88

 M_CREATBL=75
 M_CRCLBL=88
 M_ALTBL=21
 M_ASTBL=24
 M_BILIBL=8.2
 M_ALBBL=38
 M_IGGBL=9.7
 M_LDHBL=250
 M_ALPBL=90

$FIXED @annotated
g : 9.8 : Acceleration due to gravity (m/s^2)
//M_WGT : 75 : Mean body weight at baseline (kg)

$SET delta=0.1, end=240         
//$SET req=s(RESP)   request only the RESPonse compartment in the simulated output.

//$GLOBAL
//#define CP (CENT/VC)
//$GLOBAL
//bool cure = false;


//##############################################################################
// model description                                                        
//##############################################################################
//?PKMODEL
//$CMT GUT CENT PERIPH
//$PKMODEL ncmt=2, depot=TRUE


$CMT DEPOT CENTRAL PERIPH 
//$INIT GUT  = 0, CENT = 0, RESPONSE = 25
//$CMT @annotated
//DEPOT      : Dosing compartment (mg)
//CENTRAL    : Central PK compartment (mg)
//PERIPH     : Response


//##############################################################################
// derived variable, Using C++ grammar                                                     
//##############################################################################
$MAIN
  
//double CL = TVCL*pow(WT/70,WTCL)*exp(ECL);
//double V  = TVV *pow(SEXVC,SEX)*exp(EV);
//double KA = TVKA*exp(EKA);
 
  double KA = exp(log(TVKA) + ETA_KA);
  double CL0 = exp(log(TVCL)+ ETA_CLQ);
  double Q  = exp(log(TVQ)  + ETA_CLQ);  
  double V2 = exp(log(TVV2) + ETA_VSS );
  double V3 = exp(log(TVV3) + ETA_VSS);
 

  double VMAX = TVVMAX*exp(ETA_VMAX);
  double KSS  = TVKSS*exp(ETA_KSS);

  double EMAX = TVEMAX *exp(ETA_EMAX);
  double T50 = TVT50*exp(ETA_T50);   
  // double HILL = HILL

  //double PROP = RUVCV; 
  //double ADD = RUVSD;
  
  double F1   = TVF1;    //exp(TVF1)/(1+exp(TVF1)); 
 
  //;CL IS TIME-DEPENDENT VARIABLE 
  // --------------------------------------------------------------
   double  CL = CL0*exp(EMAX*pow(TIME, HILL)/(pow(T50, HILL)+pow(TIME, HILL)));     //; Sigmoid Emax model
 

  //bool COVARIATE = false; 
  // --------------------------------------
  if (COVARIATE==1) { 
    //CL = exp(log(CL) + WGT_ON_CLQ*(log(WGTBL)-log(M_WGT)));
    //V2 = exp(log(V2) + WGT_ON_VSS*(log(WGTBL)-log(M_WGT)));

 	//M_WGT = 75 ;  

  double COVCLQ=WGT_ON_CLQ *(log(WGTBL)-log(M_WGT)) +  ALT_ON_CLQ*(log(ALTBL)-log(M_ALTBL)) + ALB_ON_CLQ*(log(ALBBL)-log(M_ALBBL)) + IGG_ON_CLQ*(log(IGGBL)-log(M_IGGBL)); 
      
  
  double COVVSS=WGT_ON_VSS *(log(WGTBL)-log(M_WGT)) + BMI_ON_VSS*(log(BMIBL)-log(M_BMIBL)) ;  
  
  double COVEMAX = 0;
  
  double BLK = 0; 
  if (RACEN==2) {BLK = 1;}
  double COVT50 = BLK_ON_T50*BLK ;
  	
 	CL = CL*exp(COVCLQ);         // CL, INDIVIDUAL CLEARANCE (L/DAY)
 	Q  = Q *exp(COVCLQ);         // Q, INDIVIDUAL INTERCOMPARTMENTAL CLEARANCE (L/DAY)

 	V2 = V2*exp(COVVSS);          // V2, INDIVIDUAL CENTRAL VOLUME (L) 
 	V3 = V3*exp(COVVSS);          // V3, INDIVIDUAL PERIPHERAL VOLUME (L) 
 	
 	EMAX = EMAX*exp(COVEMAX);          // V2, INDIVIDUAL CENTRAL VOLUME (L) 
 	T50 = T50*exp(COVT50);          // V3, INDIVIDUAL PERIPHERAL VOLUME (L)  	
  }
  
  //bool SCALING = false;
  // http://www.cognigencorp.com/nonmem/nm/98dec172003.html
  // --------------------------------------------------------------
  if (SCALING==1) {
    CL = exp(log(CL) + 0.75*(log(WGTBL)-log(M_WGT)));      //CL*pow(WEIGHTBL/M_WGT, 0.75);  
    V2 = exp(log(V2) + 1.00*(log(WGTBL)-log(M_WGT)));      //V2*pow(WEIGHTBL/M_WGT,1);  
    V3 = exp(log(V3) + 1.00*(log(WGTBL)-log(M_WGT)));      //V3*pow(WEIGHTBL/M_WGT, 1);  
    Q  = exp(log(Q)  + 0.75*(log(WGTBL)-log(M_WGT)));      //Q*pow(WEIGHTBL/M_WGT, 0.75);  
    VMAX = exp(log(VMAX)  + 0.75*(log(WGTBL)-log(M_WGT)));      //Q*pow(VMAX/M_WGT, 0.75);  
  }


  
  // derived parameter
  _F(1) = F1;
  double S2   = V2;                        
  
  double K23= Q/V2;
  double K32= Q/V3;
  
  double KE  = CL/V2;
  
//##############################################################################
// model parameter (THETA, OMEGA, SIGMA)                                                    
//##############################################################################

//$THETA
//0.1 0.2 0.3

//$THETA @annotated
//0.1 : Typical value of clearance (L/hr)
//0.2 : Typical value of volume (L)
//0.3 : Typical value of ka (1/hr)


//$OMEGA @name IIV
//  labels=s(EKA,ECL,EV2,EV3,EQ,EVMAX,EKSS)
//  0.0 0.192465 0.0609905 0.338246 0 0 0   



$OMEGA @annotated @block @name CLQ_VSS
ETA_CLQ: 0.0892746: ETA on CLQ
ETA_VSS: 0.0403233 0.0412452 : ETA on VSS
 
 
$OMEGA @annotated  @name IIV_KA
  ETA_KA:   0   : IIV_KA
                      
$OMEGA @annotated  @name IIV_VMAX
  ETA_VMAX:   0   : IIV_VMAX
                        
$OMEGA @annotated  @name IIV_KSS
  ETA_KSS:  0   : IIV_KSS                       
            
$OMEGA @annotated @name IIV_EMAX
  //labels=s(ETA_EMAX)
  ETA_EMAX:   0.260328   : IIV on EMAX
                        
$OMEGA @annotated  @name IIV_T50
 //  labels=s(ETA_T50)
  ETA_T50:  0.583401  : IIV on T50


//$OMEGA @name IIV_ETA_KA
//  labels=s(ETA_KA)
//            0

//$OMEGA @name EKA
//ETA: 0.4   : ETA on KA

//$OMEGA @name EQ
//EQ:  0.02 : ETA on Q 
 
//[OMEGA] @name OMGA @correlation @block
//ECL : 1.23 : Random effect on CL
//EV  : 0.67 0.4 : Random effect on V
//EKA : 0.25 0.87 0.2 : Random effect on KA
 
//$OMEGA @name PK @block
//0.2 0.02 0.3
//$OMEGA @name PD
//0.1 0.2 0.3 0.5

//$OMEGA @correlation
//0.1 0.67 0.3

//$OMEGA @name IOV         ?modMATRIX
//  labels=s(IOVCL)
//  0 

//$OMEGA @annotated
//ECL: 0.09 : ETA on clearance
//EVC: 0.19 : ETA on volume
//EKA: 0.45 : ETA on absorption rate constant

//$OMEGA @annotated @block
//ECL: 0.09 : ETA on clearance
//EVC: 0.001 0.19 : ETA on volume
//EKA: 0.001 0.001 0.45 : ETA on absorption rate constant

 
$SIGMA @annotated  @name SGMA 
  PROP: 0.17954 : Proportional residual error
  ADD : 1.33522   : Additive residual error
//
//##############################################################################
// ODE, Using C++ grammar                                                  
//##############################################################################

$ODE       
  dxdt_DEPOT  = -KA*DEPOT;                                                                    // Free Drug depot
  dxdt_CENTRAL = KA*DEPOT-(KE+K23)*CENTRAL+K32*PERIPH-VMAX*CENTRAL/(KSS+CENTRAL/V2);         //  Free drug amount
  dxdt_PERIPH  = K23*CENTRAL-K32*PERIPH;                                            //  Free Drug second compartment amount
  
  //double INH = CP/(IMAX+CP);
  //dxdt_RESP =  KIN*(1 - INH) - RESP*KOUT;
  //SOLVERTIME

//##############################################################################
// TABLE, Using C++ grammar
//##############################################################################

$TABLE
  //double PROP = 0.25;
  //double ADD = 25;
  
  double CONC = CENTRAL/V2;
  double IPRED = CENTRAL/V2;  
  
  double TEST = 2810;
  
  //SD=sqrt(PROP*PROP+ADD*ADD/CONC**2) ;  log-transformed
  double SD=ADD+PROP*CONC;    //untransformed scale
       
  double DV = IPRED + SD;
 
//##############################################################################
// CAPTURE
//############################################################################## 
$CAPTURE CL V2 V3 Q KA F1 VMAX KSS EMAX T50 HILL COVARIATE SCALING   M_WGT  WGT_ON_CLQ  WGT_ON_VSS    ETA_CLQ  ETA_VSS  ETA_KA ETA_VMAX ETA_KSS  IPRED DV   TEST  WGTBL
//$CAPTURE @annotated
//DV  : Concentration (mg/L)
//ECL : Random effect on clearance (L/hr)
//CP  : Plasma concentration (mcg/ml)
//CL  : Individual clearance (L/hr)



