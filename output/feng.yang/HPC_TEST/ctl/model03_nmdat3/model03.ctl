;; linear + MM model
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; model4: same as MM0028 
;   2) turn off EMax model:  EMAX=0, HILL=0, T50=0
; 
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 

$PROBLEM    REGN1979 PK 

$INPUT  C=DROP CFLAG=DROP ROWID STDY ID USUBJID=DROP ARMA=DROP ARMAN COHORT=DROP COHORTN PCTPT=DROP TAD TIME NTIM DV DVOR DVORU=DROP TESTCAT=DROP TESTN TEST=DROP EVID CMT BLQ EXDUR EXDOSE AMT EXTRT=DROP EXROUTE=DROP EXROUTN RATE AGE AGEU=DROP RACE=DROP RACEN SEX=DROP SEXN ETHNIC=DROP ETHNICN WGTBL HGTBL BMIBL MDV


$DATA      ../../data/nmdat3.csv IGNORE=C   
 
$SUBROUTINE ADVAN6 TOL=6
$MODEL      COMP=(CENTRAL) ;CMT=1
            COMP=(PERIPH) ;CMT=2
$PK  

  TVCL = THETA(1)
  TVV1 = THETA(2)
  TVV2 = THETA(3)
  TVQ =  THETA(4)
  
  TVVMAX = THETA(5)
  KM = THETA(6)
  
  PROP=THETA(7)
  ADD=THETA(8)
    
  WGT_ON_CLQ = THETA(9)
  WGT_ON_VSS = THETA(10)
     
  ; COVARIATE MODEL     
  M_WT = 70 ; kg 
  COVCL = WGT_ON_CLQ *(LOG(WGTBL)-LOG(M_WT))
  COVQ  = WGT_ON_CLQ *(LOG(WGTBL)-LOG(M_WT))
  
  COVV1 = WGT_ON_VSS *(LOG(WGTBL)-LOG(M_WT))
  COVV2 = WGT_ON_VSS *(LOG(WGTBL)-LOG(M_WT))
  
  CL = TVCL*EXP(ETA(1)+COVCL)          ; CL, INDIVIDUAL CLEARANCE (L/DAY)
  Q  = TVQ *EXP(ETA(1)+COVQ)           ; Q, INDIVIDUAL INTERCOMPARTMENTAL CLEARANCE (L/DAY)
  
  V1 = TVV1*EXP(ETA(2)+COVV1)          ; V2, INDIVIDUAL CENTRAL VOLUME (L) 
  V2 = TVV2*EXP(ETA(2)+COVV2)          ; V3, INDIVIDUAL PERIPHERAL VOLUME (L) 
  
  VMAX = TVVMAX*EXP(ETA(3))   
  
  A_0(1) = 0                   ;INITIAL CONDITION FOR CENTRAL
  A_0(2) = 0                   ;INITIAL CONDITION FOR PERIPH
 
$DES 
  CP= A(1)/V1 
  DADT(1) = -(CL/V1)*A(1)-(Q/V1)*A(1)+(Q/V2)*A(2)-(VMAX*CP)/(KM+CP)
  DADT(2) =               (Q/V1)*A(1)-(Q/V2)*A(2)                                 

$ERROR  (ONLY OBSERVATIONS)  
  CONC = A(1)/V1
    
  SD=SQRT(PROP*PROP+ADD*ADD/CONC**2) ;  log-transformed
  ;SD=SQRT(ADD*ADD+PROP*PROP*CONC**2) ;  untransformed scale,  Standard deviation using proportional residual error 
 
  IPRED=LOG(CONC)  
  IRES  = DV - IPRED
  IWRES = IRES/SD
  Y     = IPRED + SD*ERR(1) 
 
$THETA  
  (0, 0.5) ; CL
  (0, 6)   ; V1
  (0, 6)   ; V2
  (0, 2)   ; Q
  (0, 0.6) ; VMAX
  (0, 0.5) ; KM
  (0, 0.3) ; PROP ERR 
  (0, 0.3) ; ADD ERR
  (0, 0.75)    ;WGT_ON_CLQ
  (0, 1.0)     ;WGT_ON_VSS  
  
$OMEGA  
  0.1  ; II_CL
  0.1  ; II_V1
  0.1  ; II_VMAX
 
 
$SIGMA  1  FIX


$ESTIMATION MAXEVAL=9999 METHOD=1 INTERACTION PRINT=1 NOABORT NSIG=2
            MSFO=001.MSF
            
$COVARIANCE PRINT=E
$TABLE      ROWID ID TIME TESTN DV DVOR IPRED MDV AMT ETA1 ETA2 ETA3   
CL V1 V2 Q VMAX KM WGT_ON_CLQ WGT_ON_VSS  PROP   ADD
            IRES IWRES CWRES NOPRINT ONEHEADER FILE=sdtab001

 