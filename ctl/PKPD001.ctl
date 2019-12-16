$SIZES PD=-150

$PROB 002, IIINFO, model with parallel linear and Michelis-Menten elimination with indirect response model for LDL-c and direct model for TG

$INPUT VVVVVVV

$DATA     ../data/DDDDDDD.csv IGNORE=C 
	  IGNORE=  GGGGGGG
	   
;ACCEPT=(DVIDN.EQ.0, DVIDN.EQ.1, DVIDN.EQ.3, DVIDN.EQ.6)   ; 0(dose), 1(EVIN), 3(TRIG), 4(LDL)    
;ACCEPT= (DVIDN.EQ.0.OR.DVIDN.EQ.1.OR.DVIDN.EQ.3)
;IGNORE= (DVIDN.EQ.2,DVIDN.GT.3,BLQ.NE.0)                      

$SUBROUTINES ADVAN13 TOL=9    ;INFN=../RunLogNM7.forz
 
$MODEL 
  COMP(Depot) 
  COMP(Central,DEFDOSE,DEFOBS) 
  COMP(Periph) 
  COMP(LDL)
 
$PK 
  IF (NEWIND.EQ.0) THEN
   LLOQ1 = 0.078 ; MG/L
  ENDIF
  
  ; PK MODEL
  CL = THETA(1)*EXP(ETA(1))           ; POPULATION CLEARANCE (L/DAY)
  V2 = THETA(2)*EXP(ETA(2))           ; POPULATION CENTRAL VOLUME (L) 
  Q  = THETA(3)*EXP(ETA(1))           ; POPULATION INTERCOMPARTMENTAL CLEARANCE (L/DAY)
  V3 = THETA(4)*EXP(ETA(2))           ; POPULATION PERIPHERAL COMPARTMENT VOLUME (L) 
  LF1  = THETA(5)                     ; POPULATION BIOAVAIBILITY 
  KA   = THETA(6)*EXP(ETA(3))         ; POPULATION ABSORPTION RATE CONSTANT (1/DAY) 
  VMAX = THETA(7)*EXP(ETA(4))
  KSS  = THETA(8)*EXP(ETA(5))
  
  RUVCV1 = THETA(9)
  RUVSD1 = THETA(10)
  RUVCV2 = THETA(11)
  RUVSD2 = THETA(12)
  RUVCV3 = THETA(13)
  RUVSD3 = THETA(14)
  
  ; PD MODEL (LDL)
  KOUT = THETA(15)*EXP(ETA(6)) 
  KIN  = LDLBL * KOUT
  IMAX = THETA(16)*EXP(ETA(7))
  IMAX = EXP(IMAX)/(1+EXP(IMAX)) 
  IC50 = THETA(17)*EXP(ETA(8)) 
  
  ; PD MODEL (TRIG) 
  EC50 = THETA(18)*EXP(ETA(9))
  EMAX = THETA(19)*EXP(ETA(10))
  GAMMA  = THETA(20)
  
  ; COVARIATE MODEL
  WGT_ON_CLQ = THETA(21)
  WGT_ON_VSS = THETA(22)
  
  M_WGT = 75 ; kg 
  COVCL = WGT_ON_CLQ *(LOG(WEIGHTBL)-LOG(M_WGT))
  COVQ  = WGT_ON_CLQ *(LOG(WEIGHTBL)-LOG(M_WGT))
  
  COVV2 = WGT_ON_VSS *(LOG(WEIGHTBL)-LOG(M_WGT))
  COVV3 = WGT_ON_VSS *(LOG(WEIGHTBL)-LOG(M_WGT))
  
  CL = CL*EXP(COVCL)          ; CL, INDIVIDUAL CLEARANCE (L/DAY)
  Q  = Q *EXP(COVQ)           ; Q, INDIVIDUAL INTERCOMPARTMENTAL CLEARANCE (L/DAY)
  
  V2 = V2*EXP(COVV2)          ; V2, INDIVIDUAL CENTRAL VOLUME (L) 
  V3 = V3*EXP(COVV3)          ; V3, INDIVIDUAL PERIPHERAL VOLUME (L) 
  
     
  ;DERIVED PARAMETERS
  K = CL/V2
  K23 = Q/V2
  K32 = Q/V3
  
  S2 = V2     ; dose = mg, conc = mg/L
  F1 = LF1    ; Or EXP(LF1)/(1+EXP(LF1))
  
  S4 = 1
  A_0(4) = LDLBL

$DES
 CONC = A(2)/S2

 DADT(1) =-KA*A(1)                                                            ; Drug depot
 DADT(2) = KA*A(1)-(K+K23)*A(2)+K32*A(3)-VMAX*A(2)/V2/(KSS+A(2)/V2)           ; Drug amount, VMAX unit: mg/day
 DADT(3) = K23*A(2)-K32*A(3)                                                  ; Drug second compartment amount
 DADT(4) = KIN*(1-IMAX*CONC/(IC50+CONC+1E-6)) - KOUT*A(4)     ; LDL-C


$ERROR (OBSERVATION ONLY) 
  CALLFL = 0
  
  CP = A(2)/S2 
  IF (CP.LE.0) CP = 0.039   
  
  LDL = A(4) 
  IF (LDL.LE.1E-1) LDL = 0.1   
  
  TRIG = TRIGBL - EMAX*(CP)**GAMMA/(EC50**GAMMA+(CP)**GAMMA)
  IF (TRIG.LE.1E-1) TRIG = 0.1 
  
  IPK = 0
  ILDL = 0
  ITRIG = 0
  IF (DVIDN.EQ.1) IPK = 1
  IF (DVIDN.EQ.3) ITRIG = 1   
  IF (DVIDN.EQ.4) ILDL = 1     
   
  IPRED = IPK*LOG(CP) + ILDL*LDL + ITRIG*TRIG 
  
  PROP1=EXP(RUVCV1); proportional part 
  ADD1=EXP(RUVSD1)/IPRED ; additive part 
  SD1=SQRT(PROP1*PROP1 + ADD1*ADD1) ; Standard deviation using proportional residual error 
  
  PROP2=EXP(RUVCV2)*IPRED; proportional part 
  ADD2=EXP(RUVSD2) ; additive part 
  SD2=SQRT(PROP2*PROP2 + ADD2*ADD2) ; Standard deviation using proportional residual error
  
  PROP3=EXP(RUVCV3)*IPRED; proportional part 
  ADD3=EXP(RUVSD3) ; additive part 
  SD3=SQRT(PROP3*PROP3 + ADD3*ADD3) ; Standard deviation using proportional residual error
    
  W = IPK*SD1 + ILDL*SD2 + ITRIG*SD3      
  Y = IPRED + IPK*SD1*EPS(1) + ILDL*SD2*EPS(2) + ITRIG*SD3*EPS(3)    
   
  IRES  = DV - IPRED
  IWRES = IRES/W
 
$THETA  
  0.105614 FIX  ;TVCL
  3.3807   FIX  ;TVV2
  0.0749863 FIX ;TVQ
  2.91904  FIX ;TVV3
  0.822791 FIX ;TVF1
  0.201545 FIX ;TVKA
  5.21901	FIX  ;TVVMAX
  0.744114  FIX ;TVKSS
  
  -1.13358  FIX  ;RUVCV1
  -27.2739  FIX  ;RUVSD1
  (-0.9)    ;RUVCV2
  (-0.9)    ;RUVSD2 
  (-0.9)    ;RUVCV3
  (-0.9)    ;RUVSD3 
  
  (0, 0.26)    ;TVKOUT  
  (0, 0.8)     ;TVIMAX   
  (0, 10)      ;TVIC50 
  
  (0, 20.7)      ;TVEC50 
  (0, 100)       ;TVEMAX 
  (0, 1)         ;TVGAMMA    
  
  0.75  FIX     ;WGT_ON_CLQ
  1  FIX        ;WGT_ON_VSS
 
 
$OMEGA  BLOCK(2) FIX
  0.0860128               ; IIV_CLQ
  0.00924939 0.0390901    ; IIV_VSS
 

$OMEGA
  1.271   FIX  ;IIV_KA
  00.0148372  FIX   ;IIV_VMAX 
  0.00413955 FIX    ;IIV_KSS
  
  0.01   ;IIV_KOUT
  0.01   ;IIV_IMAX 
  0.01   ;IIV_IC50
  0.01     ;IIV_EC50
  0.01     ;IIV_EMAX
   
  
  $SIGMA
  1 FIXED  ;SIGMA_1
  1 FIXED  ;SIGMA_2
  1 FIXED  ;SIGMA_3

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
$TABLE ROWID ID TIME DVIDN IPRED PRED DV MDV IWRES CWRES RES WRES ETA1 ETA2 ETA3 ETA4 NOAPPEND NOPRINT ONEHEADER  FILE=sdtab001  
 
;The empirical Bayes estimates of individual model parameter values, or posthoc estimates.  
$TABLE ID  CL  V2  Q V3 F1 KA VMAX KSS KIN KOUT IMAX IC50 RUVCV1 RUVSD1 WGT_ON_CLQ WGT_ON_VSS NOPRINT ONEHEADER FILE=patab001    ; patab001

;catab5: Categorical covariates, e.g. SEX, RACE.
;$TABLE ID CCCATECOVAR   NOPRINT NOAPPEND ONEHEADER FILE=catab001        

;cotab5: Continuous covariates, e.g. WT, AGE.
;$TABLE ID CCCONTCOVAR NOPRINT NOAPPEND ONEHEADER FILE=cotab001
 
;mutab5, mytab5, extra5, xptab5: Additional variables of any kind. These might be useful 
;if there are more covariates than can be accommodated in the covariates tables, for example, 
;or if you have other variables that should be added, e.g. CMAX, AUC. Value
  
