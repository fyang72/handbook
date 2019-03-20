; Model:
;   parent: 
;      1)
;      2) 
; Data: 
;   parent: 
;      1) 
;      2) 

$PROB MM001, model with parallel linear and Michelis-Menten elimination (concentration only)

$INPUT     C CFLAG=DROP ROWID STUDYID=DROP USUBJID=DROP ID ARMA=DROP VISIT=DROP TIMEPT=DROP NTIM TIME AMT RATE EVID DV CMT MDV 
TEST=DROP TESTN DVOR STDUNIT=DROP CHG PCHG CH50_BL AH50_BL C5_BL EXDOSE EXDOSU=DROP EXROUTE=DROP EXROUTN EXTDOSE 
AGE AGEU=DROP SEX=DROP SEXN RACE=DROP RACEN ETHNIC=DROP WGTBL HGTBL BMIBL


$DATA  ../../data/nmdat_PK.csv 
        IGNORE=C 
       ;IGNORE=@
       ;IGNORE=(FLAG.NE.0) 
       ;IGNORE=(NOPK.EQ.1)

$SUBROUTINES ADVAN13 TOL=6    ;ADVAN6   INFN=../RunLogNM7.for
 
$MODEL 
 COMP(Depot) 
 COMP(Central,DEFDOSE,DEFOBS) 
 COMP(Periph) 

$PK 
 IF (NEWIND.EQ.0) THEN
   LLOQ1 = 0.078 ; MG/L
 ENDIF

 TV_CL = THETA(1)           ; POPULATION CLEARANCE (L/DAY)
 TV_V2 = THETA(2)           ; POPULATION CENTRAL VOLUME (L) 
 TV_Q  = THETA(3)           ; POPULATION INTERCOMPARTMENTAL CLEARANCE (L/DAY)
 TV_V3 = THETA(4)           ; POPULATION PERIPHERAL COMPARTMENT VOLUME (L) 
 LF1   = THETA(5)           ; POPULATION BIOAVAIBILITY 
 TV_KA = THETA(6)           ; POPULATION ABSORPTION RATE CONSTANT (1/DAY) 
 TV_VMAX= THETA(7)          ; mg/day
 TV_KSS = THETA(8)
 RUVCV  = THETA(9)
 RUVSD  = THETA(10)
 
 ; COVARIATE MODEL
 WGT_ON_CLQ = THETA(11)
 WGT_ON_VSS = THETA(12)

 M_WGT = 75 ; kg 
 COV_CL = WGT_ON_CLQ *(LOG(WGTBL)-LOG(M_WGT))
 COV_Q  = WGT_ON_CLQ *(LOG(WGTBL)-LOG(M_WGT))

 COV_V2 = WGT_ON_VSS *(LOG(WGTBL)-LOG(M_WGT))
 COV_V3 = WGT_ON_VSS *(LOG(WGTBL)-LOG(M_WGT))

 CL = TV_CL*EXP(ETA(1)+COV_CL) 
 Q  = TV_Q *EXP(ETA(1)+COV_Q)  

 V2 = TV_V2*EXP(ETA(2)+COV_V2) 
 V3 = TV_V3*EXP(ETA(2)+COV_V3) 

 KA = TV_KA*EXP(ETA(3))  
 VMAX = TV_VMAX*EXP(ETA(4)) 
 KSS  = TV_KSS 

 ;DERIVED PARAMETERS
 K = CL/V2
 K23 = Q/V2
 K32 = Q/V3

 S2 = V2     ; dose = mg, conc = mg/L
 F1 = LF1    ; Or EXP(LF1)/(1+EXP(LF1))
 
;CALCULATE TIME AFTER DOSE
 IF(NEWIND.NE.2) TDOS=0
 IF(AMT.GT.0) TDOS=TIME
 TAD=TIME-TDOS         ; TIME RELATIVE TO DOSE


$DES
 DADT(1) =-KA*A(1)                                                      
 DADT(2) = KA*A(1)-(K+K23)*A(2)+K32*A(3)-VMAX*A(2)/V2/(KSS+A(2)/V2)    
 DADT(3) = K23*A(2)-K32*A(3)                                            
 

$ERROR (OBSERVATION ONLY) 
 CALLFL = 0

 EPS0 = 1E-5
 CONCI = A(2)/S2
 IF (CONCI.LE.EPS0) CONCI = EPS0
 IPRED = LOG(CONCI)

 SD=SQRT(PROP*PROP+ADD*ADD/CONC**2) ;  log-transformed
 ;SD=SQRT(ADD*ADD+PROP*PROP*CONC**2) ;  untransformed scale,  Standard deviation using proportional residual error 
  
 BLOQ1 = 0
 IF (BLOQ1.EQ.0) THEN
   F_FLAG = 0
   Y     = IPRED + SD*ERR(1)
 ELSE
   F_FLAG = 1 
   Y = PHI((LOG(LLOQ1)-LOG(CONCI))/SD)
 ENDIF
 
 IRES  = DV - IPRED
 IWRES = IRES/SD

$THETA  
 (0,0.15)  ;TV_CL
 (0,3.00)  ;TV_V2
 (0,0.45)  ;TV_Q
 (0,1.50)  ;TV_V3
 (0,0.70)  ;TV_F1
 (0,0.28)  ;TV_KA
 (0,1.78)  ;TV_VMAX
 (0,0.1)   ;TV_KSS
 (-0.9)    ;RUVCV
 (-0.9)    ;RUVSD
 (0.75)    ;WGT_ON_CLQ
 (1.0)     ;WGT_ON_VSS


$OMEGA  BLOCK(2)
 0.1341               ; IIV_CLQ
 0.0159127 0.0417008  ; IIV_VSS

  
$OMEGA
 0.01     ;IIV_KA
 0.01     ;IIV_VMAX

 
$SIGMA
0.01   ;SIGMA_1
 

$EST MAXEVAL=9999 NSIG=3 SIGL=9 METHOD=1 PRINT=10 NOABORT POSTHOC MSFO=001.MSF NOTHETABOUNDTEST NOOMEGABOUNDTEST NOSIGMABOUNDTEST  
 
$COV PRINT=E UNCONDITIONAL MATRIX=S SIGL=12

;$SIMULATION (20102) (330333 UNIFORM) ONLYSIM SUBPROBLEMS=1


;$EST   METHOD=ITS INTERACTION LAPLACIAN NITER=100 NSIG=3 SIGL=9 CTYPE=2 CINTERVAL=50 NOABORT PRINT=5  GRD=TS(7) NOCOV=1 FILE=blq.ext
;$EST   METHOD=SAEM INTERACTION LAPLACIAN NBURN=10000 NITER=1000 CTYPE=2 PRINT=50 NOCOV=1   CINTERVAL=50 ISAMPLE=2 GRD=TS(7) FILE=blq_saem.ext
;$EST   METHOD=IMP INTERACTION LAPLACIAN ISAMPLE=2000 NITER=10 EONLY=1 PRINT=1 MAPITER=0 NOCOV=0       GRD=TS(7) DF=0    
;$COV   MATRIX=R COMPRESS UNCOND PRINT=E


;************************************************************
;**** TABLES ****
;************************************************************ 
;The 'standard' parameters, including IWRE, IPRE, TIME, and the NONMEM default items (DV, PRED, RES and WRES)
$TABLE ROWID ID TIME IPRED PRED DV MDV IWRES CWRES RES WRES ETA1 ETA2 ETA3 ETA4 NOAPPEND NOPRINT ONEHEADER  FILE=sdtab001  

;The empirical Bayes estimates of individual model parameter values, or posthoc estimates.  
$TABLE ID  CL  V2  Q V3 F1 KA VMAX KSS  RUVCV RUVSD WGT_ON_CLQ WGT_ON_VSS NOPRINT ONEHEADER FILE=patab001    ; patab001

;catab5: Categorical covariates, e.g. SEX, RACE.
;$TABLE ID ARMAN SEXN RACEN ETHNICN     NOPRINT NOAPPEND ONEHEADER FILE=catab001        

;cotab5: Continuous covariates, e.g. WT, AGE.
;$TABLE ID  AGE  WGTBL HGTBL BMIBL NOPRINT NOAPPEND ONEHEADER FILE=cotab001
 
;mutab5, mytab5, extra5, xptab5: Additional variables of any kind. These might be useful 
;if there are more covariates than can be accommodated in the covariates tables, for example, 
;or if you have other variables that should be added, e.g. CMAX, AUC. Value
  
