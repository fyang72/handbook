# From: Yi Zhang 
# Sent: Wednesday, January 16, 2019 10:17 AM
# To: Ronda Rippley <ronda.rippley@regeneron.com>; Lisa Lin <kuanju.lin@regeneron.com>
#   Subject: Re: CD55 BB
# 
# Tom also asked to see few things: individual concentration vs total C5 to see if 
# C5 has plateaued on the individual level; PK simulation without considering the 
# effect of baseline C5. I will see if there are already information to address 
# his request. If not, I can do the first request with R. The new simulation, 
# how should we respond? I wanted to see if Feng has done any analysis to quantify
# the effect of C5, maybe that will be adequate to show the impact inclusion of C5 
# in the simulation. Please let me know your thoughts. Thanks.
# 



#----------------------------------------------------------------------------
  # Version 0.1   Created on 11/08/2018, Feng Yang
  #----------------------------------------------------------------------------
  
  # https://www.psiweb.org/docs/default-source/2018-psi-conference-posters/02-alex-carlton.pdf?sfvrsn=ff68dedb_4
  
  
 # http://www.pmxsolutions.com/2018/08/06/nonmem-model-fitting-as-a-gif-in-r-with-code/
  
    
    
  ####################################################################
  #Olivier Harari <olivier.harari@regeneron.com>, Thursday, July 19, 2018 5:08 PM
  # Couple points for your follow up Yan:
  #   1.	The transient fall in total C5 we see upon 3918 infusion could be a prozone 
  # effect where by a massive excess of non-interfering antibodies in a soluble phase 
  # sandwich ELISA interferes not with binding of detection antibody but with lattice formation.
  # 2.	We would expect R3918/ECU/C5 immune complexes to be non-complement fixing because
  # of the Fc isotypes. Most circulating immune complex assays (CIC) use complement fixation 
  # as their read out. However there are assays whereby CIC are column separated or 
  # immunoprecipitated using anti-Fc antibodies and then sized by gradient 
  # centrifugation/chromatography/electrophoresis. That said: how neat would this be? –
  #a sandwich ELISA for CIC specific for us – capture with anti-Ecu idiotype and detect with anti-3918 idiotype..
    # 

  #-------------------------------------------------------------------
  # setup 
  #-------------------------------------------------------------------
  HOME =  "~/FYANG/R3918_C5/KRM/"
  #HOME = "C:/FYANG/R3918/"
  setwd(HOME)
  
  idebug = 0
  source(paste0(dirname(dirname(HOME)), "/global.R"))
  
  # extra temporary util functions, once mature, will be included in the regnR library.
  source("./util.R")
  
  # detach some library (if loaded) which may cause conflicts.
  base::detach(package:MASS)
  base::detach(package:reshape)
  
  # some global variables that will be used in subsequent analysis.
  STUDY.NAME = "Study R3918-HV-1659"
  
  DRUG.NAME = "Pozelimab"   # REGN3918"
  
  PK.TEST.NAME = "REGN3918"
  PK.TEST.LABEL = "Total Pozelimab"   # Total REGN3918"
  
  TARGET.TEST.NAME = "C5"
  TARGET.TEST.LABEL = "Total C5"
  
  PD.TEST.NAME = "CH50H"
  PD.TEST.LABEL = "CH50"
  
  PD.PCHG.LABEL = "Percent Change from Baseline in CH50"
 
  
  #-------------------------------------------------------------------
  # run all the scripts to generate the doc 
  #-------------------------------------------------------------------
  idebug = 0
  
  FIGURE_ALL = NULL
  TABLE_ALL = NULL
  LISTING_ALL = NULL
  
  source("./S10_basicVisualization.R")
  source("./S11_PKPD_Relationship.R")
  source("./S12_PK_Target_Relationship.R")
  source("./S20_basicTabular.R")
  
  #source("./S30_modelDiagnostic.R")
  
  source("./S42_postHoc.R")  
  #source("./S50_runSim.R")
    
  ids = which(substr(names(FIGURE_ALL), 1, 6)!="canvas")
  FIGURE_ALL = FIGURE_ALL[ids]
  
  # use customized version
  myppt <- pptx()   #title = "title", template = '~/FYANG/LIB/pptTemplate.pptx')
  #mydoc <- docx(template = "./docs/Pharmacometric Memo-Template.docx", empty_template = FALSE)
  mydoc <- docx(template = "./docs/PMx Memo-Template.docx", empty_template = FALSE)  
  
  tt = print2_word_ppt(FIGURE_ALL, TABLE_ALL,  mydoc, myppt) 
  
  writeDoc(tt$mydoc, file = './docs/memo_results.docx')
  
  #writeDoc(tt$myppt, file = './docs/ppt_results.pptx')
  
   

