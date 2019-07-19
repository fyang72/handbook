


if (idebug ==1) {
  FIGURE_ALL = NULL
  TABLE_ALL = NULL
}


if (1==2) { 
sdtab = read.table("./PKreport/ctl/SIGMOID001/sdtab001", skip=1, header=TRUE)
tdata = sdtab %>% mutate(PCHG = (CH50-CH50_BL)/CH50_BL*100, 
                         GAMMA = 4,
                         IPRED2 = E0 - EMAX*CONC**GAMMA/(CONC**GAMMA+EC50**GAMMA),
                         PCHG_SIM= (IPRED2-CH50_BL)/CH50_BL*100 
)

head(tdata)

hist(tdata$EC50)
ggplot(tdata, aes(x=CONC, y=PCHG)) + geom_point(col="black") + scale_x_log10() + 
  geom_point(data=tdata, aes(x=CONC, y=PCHG_SIM),col="blue")   

}


# load mrgsolve cpp model
#-------------------------------------
library(dplyr) 


# theta.names <- names(THETA)
# OMEGA <- matrix(0, nrow=length(THETA),ncol=length(THETA))
# rownames(OMEGA) <- colnames(OMEGA) <- theta.names 
# OMEGA[rownames(t.1),colnames(t.1)] <- t.1

#Calculate individual parameter values
E0 = 0 
EMAX = 100  
  
THETA = 29.916
GAMMA =  5   # 2.37086
OMEGA = 0.126282

ID = seq(1, 1000)
#library(MASS)
ETA <- MASS::mvrnorm(n=length(ID),mu=rep(0,length(THETA)),Sigma=OMEGA)
#base::detach(package:MASS)


CP =  unique(sort(c( 10^(seq(-1, 3, by=0.1) ), 
                     seq(100, 200, by=50))))


tdata = expand.grid(ID=ID, CP=CP) %>% left_join(data.frame(ID=ID, ETA=ETA), by="ID")

tdata = tdata %>% mutate(EC50 = THETA*exp(ETA), 
                         CH50 = E0 - EMAX*CP^GAMMA/( EC50^GAMMA +CP^GAMMA)
)
tdata0 = tdata

range(tdata$EC50)
hist(tdata$EC50)


tdata = tdata %>% calc_stats(id = "ID", group_by=c("CP"), value="CH50")  

 
fig = ggplot(tdata , aes(x=CP, y=Mean))  + geom_line() + # + geom_point()
  geom_ribbon(data=tdata,aes(ymin=PCT2P5,ymax=PCT97P5),alpha=0.3) + 
   geom_vline(xintercept = c(THETA ), lty="dashed")  + 
   theme_bw() + 
   xlab("Concentration of REGN3918 (mg/L)") + 
   ylab("Percent Change from Baseline in CH50") + 
   #scale_y_log10() + 
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))  
    
 fig =  fig + annotation_logticks(sides ="b")  # "trbl", for top, right, bottom, and left.

fig

 
# percentage of population above theshold concentration  
tdata = tdata0 %>% filter(CP %in% c(100, 150, 200))  %>% 
  mutate( 
         CH50_EC99 = ifelse(CH50<(-99), 1, 0),
         CH50_EC95 = ifelse(CH50<(-95), 1, 0), 
         CH50_EC90 = ifelse(CH50<(-90), 1, 0), 
         CH50_EC80 = ifelse(CH50<(-80), 1, 0)
  )
tabl = tdata%>% group_by(CP) %>% summarise(N=length(unique(ID)), 
                         PCT_CH50_EC99 = paste0(round(sum(CH50_EC99)/N*100,digits=2),"%"),
                         PCT_CH50_EC95 = paste0(round(sum(CH50_EC95)/N*100,digits=2),"%"),
                         PCT_CH50_EC90 = paste0(round(sum(CH50_EC90)/N*100,digits=2),"%"), 
                         PCT_CH50_EC80 = paste0(round(sum(CH50_EC80)/N*100,digits=2),"%")
)
tabl

attr(tabl, 'title') <- "Percentage  of Subject Achieving -99%, -95%, -90% and -80% Reduction in CH50 at Given Conc" 
TABLE_ALL[["Summary_CH50_PCHG_Given_Conc"]]  = tabl



  
##########################################################################################
  tdata = adpx.v0 
  
  tdata = tdata %>% mutate(TIMEPT = gsub("DAY 1 15IN", "DAY 1 15MIN", TIMEPT, fix=TRUE), 
                           TIMEPT = gsub("DAY 1 30IN", "DAY 1 30MIN", TIMEPT, fix=TRUE)) 
  
  tdata = tdata %>% mutate(DVOR = as_numeric(DVOR)) %>% 
    mutate(CHG = NA, PCHG = NA)  
  
  
  # percernt of changes in CH50
  t1 = tdata %>% filter(TEST=="CH50", TIMEPT=="DAY 1 PRE") %>% rename(CH50_BL = DVOR)
  
  tdata = tdata %>% left_join(t1 %>% dplyr::select(USUBJID, CH50_BL), by="USUBJID") %>% 
    mutate(CHG = ifelse(TEST=="CH50", DVOR-CH50_BL, CHG),
           PCHG = ifelse(TEST=="CH50", (DVOR-CH50_BL)/CH50_BL*100, PCHG)
    )
  
  
  # percernt of changes in AH50
  t1 = tdata %>% filter(TEST=="AH50", TIMEPT=="DAY 1 PRE") %>% rename(AH50_BL = DVOR)
  tdata = tdata %>% left_join(t1 %>% dplyr::select(USUBJID, AH50_BL), by="USUBJID")   %>%
    mutate(CHG = ifelse(TEST=="AH50", DVOR-AH50_BL, CHG),
           PCHG = ifelse(TEST=="AH50", (DVOR-AH50_BL)/AH50_BL*100, PCHG)
    )
  
  
  # percernt of changes in PK_C5
  t1 = tdata %>% filter(TEST=="PK_C5", TIMEPT=="DAY 1 PRE") %>% rename(C5_BL = DVOR)
  tdata = tdata %>% left_join(t1 %>% dplyr::select(USUBJID, C5_BL), by="USUBJID")  %>%
    mutate(CHG = ifelse(TEST=="PK_C5", DVOR-C5_BL, CHG),
           PCHG = ifelse(TEST=="PK_C5", (DVOR-C5_BL)/C5_BL*100, PCHG)
    )
  
  adpx = tdata    #  all plots were generated using "adpx" #################################
  
  
  
  tdata = adpx %>% filter(TEST %in% c("PK_Serum", "CH50")) %>%   filter(!is.na(DVOR))
  tdata = tdata %>% mutate(DVOR = ifelse(TEST=="PK_Serum", as_numeric(DVOR)/1000, DVOR))  %>% 
    mutate(DVOR = as_numeric(DVOR))
  
  arma.lst = c( "Placebo",  "1 mg/kg IV", "3 mg/kg IV", "10 mg/kg IV", "30 mg/kg IV",
                "300 mg SC",  "600 mg SC",   "15mkg IV+400 mg SC")           
  tdata = tdata %>% mutate(ARMA = gsub("-", " ", ARMA, fix=TRUE), 
                           ARMA = ordered(ARMA, levels = arma.lst ))
  
  
  tdata = tdata %>% distinct(STUDYID, USUBJID, ARMA, VISIT, TIMEPT, TEST, DVOR, SAMDTTM, .keep_all=TRUE) %>% 
    arrange(USUBJID, NTIM)  %>% 
    dplyr::select(USUBJID, ARMA, VISIT, TIMEPT,  TEST, DVOR, CH50_BL)  %>% 
    filter(!is.na(ARMA))
  # 
  # USUBJID         ARMA   VISIT TIMEPT  TEST  DVOR
  # <chr>        <ord>   <chr>  <chr> <chr> <dbl>
  #   1 R3918-HV-1659-826001169 400 mg SC qw VISIT 3 DAY -1  CH50 0.199
  #   2 R3918-HV-1659-826001169 400 mg SC qw VISIT 3 DAY -1  CH50 0.202
  tdata = tdata %>% filter(! (USUBJID=="R3918-HV-1659-826001169" & TEST=="CH50" & DVOR==202)) 
  
  
  ###########################################
  tdata = tdata %>% spread(TEST, DVOR)  %>% 
    mutate(PCHG = (CH50-CH50_BL)/CH50_BL*100)
#tdata = read_csv("./PKreport/tabl/R3918-HV-1659_forShinyPMx.csv")
fig = fig + geom_point(data = tdata , aes(x=as_numeric(PK_Serum   ), y=as_numeric(PCHG), col=ARMA))  + 

  theme_bw() + 
  #scale_colour_manual(values = c("black", "blue", "green")) + 
  
  ggplot2::theme(legend.position = "bottom", legend.title = element_blank()) + 
  base_theme(font.size = as.integer(12)) + 
  guides(col=guide_legend(ncol=4,byrow=TRUE)) +  
  
  ggplot2::theme(  #panel.grid.minor = element_line(colour = "gray95",size=0.75), 
    panel.grid.major = element_line(colour = "gray97",size=0.75)  )  



attr(fig, 'title') <-"Percent Change from Baseline in CH50 vs Concentration of REGN3918 with a Sigmoid Model Fit"

FIGURE_ALL[["Sigmoid_fit_PKPD"]] = fig 



