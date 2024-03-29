$PROB  THEOPHYLLINE   POPULATION DATA
$INPUT C=DROP ID AMT TIME DV WGTBL SEXN
$DATA       ../../data/DAT1.csv  IGNORE=C

$SUBROUTINES  ADVAN2

$PK
;THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)
;THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)
;THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/KG)
;SCALING PARAMETER=VOLUME/WGTBL SINCE DOSE IS WEIGHT-ADJUSTED
   CALLFL=1
   KA=THETA(1)+ETA(1)
   K=THETA(2)+ETA(2)
   CL=THETA(3)*WGTBL+ETA(3)
   SC=CL/K/WGTBL
SEX = 1

$THETA  (.1,3,5) (.008,.08,.5) (.004,.04,.9)
$OMEGA BLOCK(3)  6 .005 .0002 .3 .006 .4

$ERROR
  IPRED = F   
  IRES = DV-IPRED
  ;PROP=SQRT(SIGMA(1,1))*IPRED
  ;ADD=0 ; SQRT(SIGMA(2,2))
  ; COV = SIGMA(1,2); if covariance is required between two epsilons
  ; SD=SQRT(PROP*PROP + ADD*ADD + 2*COV)  ; if covariance is required between two epsilons
  ;SD=SQRT(PROP*PROP + ADD*ADD)
  
  SD = SQRT(SIGMA(1,1))
  IWRES = IRES/SD
  Y = IPRED+EPS(1)
  
$SIGMA  .4




$EST MAXEVAL=9999 NSIG=3 SIGL=9 METHOD=1 PRINT=10 NOABORT POSTHOC MSFO=001.MSF NOTHETABOUNDTEST NOOMEGABOUNDTEST NOSIGMABOUNDTEST  
 
$COV PRINT=E UNCONDITIONAL MATRIX=S SIGL=12
 


;************************************************************
;**** TABLES ****
;************************************************************ 
$TABLE ID TIME DV MDV EVID IPRED IWRES CWRES ONEHEADER NOPRINT FILE=sdtab001
$TABLE ID CL  KA ETA1 ETA2 ETA3 ONEHEADER NOPRINT FILE=patab001
$TABLE ID ONEHEADER NOPRINT FILE=catab001
$TABLE ID WGTBL ONEHEADER NOPRINT FILE=cotab001
$TABLE ID CL KA WGTBL ETA1 ETA2 ETA3 ONEHEADER NOPRINT FILE=mytab001







