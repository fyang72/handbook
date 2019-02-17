$PROB  THEOPHYLLINE   POPULATION DATA
$INPUT      C=DROP ID DOSE=AMT TIME CP=DV WT
$DATA       ../../data/THEOPPcopy.csv  IGNORE=C

;ADVAN2 One Compartment Linear Model with First Order Absorption

$SUBROUTINES  ADVAN2

$PK
;THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)
;THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)
;THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/KG)
;SCALING PARAMETER=VOLUME/WT SINCE DOSE IS WEIGHT-ADJUSTED

   CALLFL=1
   KA=THETA(1)+ETA(1)
   K=THETA(2)+ETA(2)
   CL=THETA(3)*WT+ETA(3)
   SC=CL/K/WT
SEX = 1

$THETA  
(.1, 1, 5)   ; KA
(.008, .08, 1.5)  ; K
(.004, .04, 1.9)  ; CL


$OMEGA BLOCK(3)  6 .005 .0002 .3 .006 .4

$ERROR
  IPRED = F   
  IRES = DV-IPRED
  PROP=SQRT(SIGMA(1,1))*IPRED
  ADD=SQRT(SIGMA(2,2))
  ; COV = SIGMA(1,2); if covariance is required between two epsilons
  ; SD=SQRT(PROP*PROP + ADD*ADD + 2*COV)  ; if covariance is required between two epsilons
  SD=SQRT(PROP*PROP + ADD*ADD)
  
  IWRES = IRES/SD
  Y = IPRED+IPRED*EPS(1) + EPS(2)

$SIGMA
0.02 ; proportional error
0.1 ; additive error

$EST METHOD=1 INTERACTION MAXEVAL=9999 SIG=3 PRINT=5 NOABORT POSTHOC MSFO=msf100
  
 
$COV PRINT=E UNCONDITIONAL MATRIX=S SIGL=12
 


;************************************************************
;**** TABLES ****
;************************************************************ 
;The 'standard' parameters, including IWRE, IPRE, TIME, and the NONMEM default items (DV, PRED, RES and WRES)
;$TABLE ID TIME   PRED DV MDV CWRES RES WRES ETA1 ETA2 ETA3  NOAPPEND NOPRINT ONEHEADER  FILE=sdtab001  

;The empirical Bayes estimates of individual model parameter values, or posthoc estimates.  
;$TABLE ID  CL  KA  NOPRINT ONEHEADER FILE=patab001     

$TABLE ID TIME DV MDV EVID IPRED IWRES CWRES ONEHEADER NOPRINT FILE=sdtab001
$TABLE ID CL  KA ETA1 ETA2 ETA3 ONEHEADER NOPRINT FILE=patab001
$TABLE ID ONEHEADER NOPRINT FILE=catab001
$TABLE ID WT ONEHEADER NOPRINT FILE=cotab001
$TABLE ID CL KA WT ETA1 ETA2 ETA3 ONEHEADER NOPRINT FILE=mytab001
