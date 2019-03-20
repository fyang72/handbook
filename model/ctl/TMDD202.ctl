$PROB QE-TMDD model for human studies
; 1) remove all FIX parameters (TMDD201.ctl), [~12 minutes]
; 2) re-write M3 method and error structure to avoid the nest if error, 
;    also used nmdat2.csv (add column of BLOQ)

;;-------------------------------------------------------------------------------------;;
;; QE-TMDD model with 2 binding sites (Gibianski 2017)                                 ;;
;; log Additive + proportional error model                                             ;;
;; IIV in Kin and Kout                                                                 ;;
;;                                                                                     ;;
;; Final PK model: With WTKG on CL [pow]                                               ;; 
;; Covariance estiamted on IIV in CL, V2 and V2, KA                                    ;;
;;                                   						       ;;
;; QE-TMDD model for human							       ;;
;; PK parameter fixed to the final pk model: ../final/final-pk-model-01.ctl            ;;
;;                                                                                     ;;
;; No IIV on KOUT                                                                      ;;
;;-------------------------------------------------------------------------------------;;
 
 
$INPUT ROWID ID ARMAN NTIM TIME AMT RATE EVID DV ODV CMT MDV TESTN EXDOSE ROUTIV	
       EXTDOSE AGE SEXN	RACEN WTKG HGTBL BMIBL CONC DELFLAG BLOQ


$DATA   ../../data/nmdat2.csv
IGNORE=@
IGNORE=(DELFLAG.EQ.1)


$THETA   
 (0.270993 )    ;th1 - TVCL: CLEARANCE (L/Day)
 (3.08523 )     ;th2 - TVV2: Central volume of distribution (L)
 (0.0181932 )   ;th3 - TVQ: Intercompartmental clearance (L/Day)
 (0.366136 )    ;th4 - TVV3: Peripheral volume of distribution (L)
 (0.657177 )    ;th5 - TVF1: Bioavailability
 (0.151185 )    ;th6 - TVKA: First-order absorption rate constant (1/Day)
 (-1.11142 )    ;th7 - Additive RV term
 (-2.52728 )    ;th8 - Proportional RV term
 (0.951618 )    ;th9 - Weight effect pm CL
 (0,500)           ;th10 - KD: Equilibrium dissociation constant (mg/L)
 (0,15)            ;th11 - KIN: Protein synthetic constant (mg/L/Day)
 (0,0.2)           ;th12 - KOUT: Protein degradation rate (1/Day)
 

 
 
$OMEGA BLOCK(3) 
 0.0434947                         ;eta1 - IIV in CL
 -0.0149456 0.00702114             ;eta2 - IIV in V2
 0          -0.0147029  0.114653   ;eta3 - IIV in KA

$OMEGA
 0.0618751                      ;eta4 - IIV in V3
 1.5                               ;eta5 - IIV in KD
 0.01                              ;eta6 - IIV in Kin
 0.01                                ;eta7 - IIV in Kout
  
   

$SIGMA
1            ;eps1 - RV for PK  [add]
0.1             ;eps2 - RV for PD [add]
 
 
 
$SUBROUTINES ADVAN13 TOL=9    
 
$MODEL NCOMP=4
 COMP(Depot) 
 COMP(Central,DEFOBS) 
 COMP(Periph) 
 COMP(Protein)
 

$PK 
 
 IF (NEWIND.EQ.0) THEN
 LLOQ1=0.078
 ENDIF


 MWTKG = 68.5   ;median weight (kg)
 MAGE = 34      ;median age (y)
 
 SEXF=0
 IF (SEXN.EQ.2) SEXF=1
 
 RACW=0
 IF (RACEN.EQ.1) RACW=1

 TVCL = THETA(1)*(WTKG/MWTKG)**THETA(9)
 CL = TVCL*EXP(ETA(1))
 
 TVV2 = THETA(2)
 V2 = TVV2*EXP(ETA(2))
 
 TVQ  = THETA(3)
 Q = TVQ
 
 TVV3 = THETA(4)
 V3 = TVV3*EXP(ETA(4))
 
 F1  = THETA(5)
 
 TVKA = THETA(6)
 KA = TVKA*EXP(ETA(3))

 TVKD = THETA(10)
 KD = TVKD*EXP(ETA(5))

 TVKIN = THETA(11)
 KIN = TVKIN*EXP(ETA(6))
 
 TVKOUT = THETA(12)
 KOUT = TVKOUT*EXP(ETA(7))
 

 BASE = KIN/KOUT

 A_0(4) = BASE   ;mean predose C5 concentration from the population (uM)
 
 
 ;DERIVED PARAMETERS
 K = CL/V2
 K23 = Q/V2
 K32 = Q/V3

 S2 = V2     ; AMT = mg, conc = mg/L

 ; Or EXP(LF1)/(1+EXP(LF1))
 
;CALCULATE TIME AFTER DOSE
 IF(NEWIND.NE.2) TDOS=0
 IF(AMT.GT.0) TDOS=TIME
 TAD=TIME-TDOS         ; TIME RELATIVE TO DOSE


$DES 
 R = 0.5*(-(2*(A(2)/V2)+KD-A(4))+SQRT((2*(A(2)/V2)+KD-A(4))**2 + 4*KD*A(4)))                                ; Free target [conc]
 CONC1 = (A(2)/V2)*KD*KD/((KD+R)*(KD+R))                                                                    ; Drug in central cmt [conc]
 
 RC = 2*KD*(A(2)/V2)*R/((KD+R)**2)
 R2C = (A(2)/V2)*R*R/((KD+R)**2)
 
 DADT(1) = -KA*A(1)                                                                                         ; Drug depot [mass]
 DADT(2) = KA*A(1)+ K32*A(3) - (K+K23)*CONC1*V2 - (K*(A(2)/V2)*R*(2*KD+R)/((KD+R)*(KD+R)))*V2               ; Total Drug [mass]
 DADT(3) = K23*CONC1*V2 - K32*A(3)                                                                          ; Drug in second compartment [mass]
 DADT(4) = KIN - KOUT*R-K*(A(2)/V2)*2*R/(KD+R)                                                              ; Total protein [conc]
 

 
$ERROR (OBSERVATION ONLY) 
 CALLFL = 0
 
 ;IPRED = LOG(F)
 ;W = SQRT(THETA(7)**2+THETA(8)**2/F**2)
 ;IRES = DV - IPRED
 ;IWRES = IRES/W
 ;Y = IPRED + W*EPS(1)
 
 EPS0 = 1E-5
 ;CP = 0.5*(A(2)/V2 - A(4)-KD) + 0.5*SQRT((A(2)/V2-A(4)-KD)**2 + 4*KD*A(2)/V2)
 CP = A(2)/S2
 IF (CP.LE.EPS0) CP = EPS0
 
 PD = A(4)
 
 PROP = EXP(THETA(8))*LOG(CP)
 ADD = EXP(THETA(7))
 SD1 = SQRT(PROP*PROP + ADD*ADD)
 SD2 = 1
   
 IPK = 0
 IPD = 0
 IF (CMT.EQ.2) IPK = 1
 IF (CMT.EQ.4) IPD = 1
 
 IPRED = IPK*LOG(CP) + IPD*LOG(PD) 
  
 W = IPK*SD1 +  IPD*SD2      
       
 LLOQ = 0.078 ; mg/L
 IF (BLOQ.EQ.0) THEN
   F_FLAG = 0
   Y     = IPK*LOG(CP)+IPK*SD1*EPS(1) +  IPD*LOG(PD)+IPD*SD2*EPS(2) 
 ELSE
   F_FLAG = 1 
   Y = IPK*PHI((LOG(LLOQ)-LOG(CP))/SD1)
 ENDIF  
   
 
 IRES = DV-IPRED
 IWRES = IPK*IRES/SD1 + IPD*IRES/SD2
 
 
 
$EST MAXEVAL=9999 NSIG=3 SIGL=9 METHOD=1 PRINT=5 NOABORT  NOTHETABOUNDTEST NOOMEGABOUNDTEST NOSIGMABOUNDTEST  
 
$COV PRINT=E MATRIX=S

;$SIMULATION (20102) (330333 UNIFORM) ONLYSIM SUBPROBLEMS=1


;$EST   METHOD=ITS INTERACTION LAPLACIAN NITER=100 NSIG=3 SIGL=9 CTYPE=2 CINTERVAL=50 NOABORT PRINT=5  GRD=TS(7) NOCOV=1 FILE=blq.ext
;$EST   METHOD=SAEM INTERACTION LAPLACIAN NBURN=10000 NITER=1000 CTYPE=2 PRINT=50 NOCOV=1   CINTERVAL=50 ISAMPLE=2 GRD=TS(7) FILE=blq_saem.ext
;$EST   METHOD=IMP INTERACTION LAPLACIAN ISAMPLE=2000 NITER=10 EONLY=1 PRINT=1 MAPITER=0 NOCOV=0       GRD=TS(7) DF=0    
;$COV   MATRIX=R COMPRESS UNCOND PRINT=E


;************************************************************
;**** TABLES ****
;************************************************************ 
;The 'standard' parameters, including IWRE, IPRE, TIME, and the NONMEM default items (DV, PRED, RES and WRES)
$TABLE ROWID ID ARMAN TIME TAD CMT EVID IPRED PRED DV CP R RC R2C CONC1 MDV IWRES CWRES RES WRES TVCL CL TVV2 V2 TVQ Q TVV3 V3 TVKA KA F1 WTKG ROUTIV
       TVKD KD TVKIN KIN TVKOUT KOUT ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 ETA7
       NOAPPEND NOPRINT ONEHEADER  FILE=sdtab001  

;The empirical Bayes estimates of individual model parameter values, or posthoc estimates.  
;$TABLE ID  TVCL CL TVV2 V2 TVQ Q TVV3 V3 TVKA KA KD KIN KOUT F1 WTKG 
       ;NOPRINT ONEHEADER FILE=patab001   
 
;catab5: Categorical covariates, e.g. SEX, RACE.
;$TABLE ID ARMAN SEXN RACEN ETHNICN     NOPRINT NOAPPEND ONEHEADER FILE=catab001        

;cotab5: Continuous covariates, e.g. WT, AGE.
;$TABLE ID  AGE  WGTBL HGTBL BMIBL NOPRINT NOAPPEND ONEHEADER FILE=cotab001
 
;mutab5, mytab5, extra5, xptab5: Additional variables of any kind. These might be useful 
;if there are more covariates than can be accommodated in the covariates tables, for example, 
;or if you have other variables that should be added, e.g. CMAX, AUC. Value
  
