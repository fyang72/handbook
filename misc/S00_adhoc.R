# based on MM0024, C:\documents\REGN1979\popPK\data\fromFeng

remove(list=ls())
library(mrgsolve)
library(dplyr)
# library(plyr)
library(ggplot2)
library(reshape2)
library(metrumrg)

previous_theme<-theme_set(theme_bw())

po.nopanel <- list(theme(
  #panel.grid.minor=theme_blank(),
  #panel.grid.major=theme_blank(),
  axis.title.x = element_text(size = 16,face="bold"), 
  axis.title.y = element_text(size = 16,face="bold",angle=90),
  axis.text.x=element_text(size=12),
  axis.text.y=element_text(size=14),
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 16),
  strip.text.x = element_text(size = 16,face="bold")))

log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks = 
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}




code <- '
$GLOBAL
#define CP (CENT/VC)
#define CT (PERIPH/VP)



$SET delta=0.1

$PARAM  TVCL=0.772, TVVC=4.91, TVVP = 3.69,TVQ=0.566, TVVMAX=8030, TVKM=803,TVEMAX=-4.47, TVT50=3.57, TVHILL=0.864


$CMT CENT PERIPH

$MAIN

double CL = exp(log(TVCL) + ETA(1));
double VC = exp(log(TVVC) + ETA(2));
double VMAX = exp(log(TVVMAX) + ETA(3));
double EMAX = TVEMAX *exp(ETA(4));       
double T50 =  TVT50 * exp(ETA(5));
double HILL = TVHILL;
double KM = TVKM;
double Q = TVQ;
double VP = TVVP;
double TDVM=VMAX*exp(EMAX*pow(TIME, HILL)/(pow(T50, HILL)+pow(TIME, HILL))); 


double CLNL=TDVM/(KM+CP);


$OMEGA 0.449 0.284 2.22 0.218 6.90

$SIGMA 
1

$ODE

dxdt_CENT = -CL*CP-Q*CP + Q*CT-TDVM*CP/(KM+CP);
dxdt_PERIPH =      Q*CP - Q*CT;

$TABLE 

double CLT = CL+CLNL;

$CAPTURE 
TIME CP CL CLT TDVM CL VC VMAX EMAX T50 HILL KM Q VP 
'
####################################



####################################


mod1 <- mcode("R1979_1", code)

source("./MODEL/cpp/mod_R1979.cpp")
mod2 <- mread("R1979_2", tempdir(),mymodel)     # ?mread , ?mcode



tdata1 = mod1 %>%
  drop.re %>%
  ev(amt=5000) %>%
  # Req(CP) %>%
  mrgsim(end=77)
  
  
tdata2 = mod2 %>%
  drop.re %>%
  ev(amt=5000, cmt=2) %>%
  # Req(CP) %>%
  mrgsim(end=77)


head(tdata1) 

head(tdata2)






CLNL0=0.924

#################   popPK           ###########

# dose for 11N cohort

# note that time is in day now
# split dose at day 0,1,7,8,14,15, 4hr infusion qual 0.167 day
e1<-data.frame(amt=c(500,500,3000,3000,6000,6000),time=c(0,1,7,8,14,15),evid=1,ii=0,addl=0,cmt=1,
               rate=c(500,500,3000,3000,6000,6000)/0.167)
e2<-data.frame(amt=12000,ii=7,addl=8,time=21,evid=1,cmt=1,rate=12000/0.167)
e3<-data.frame(amt=12000,ii=14,addl=11,time=91,evid=1,cmt=1,rate=12000/0.167)

e4<-rbind(rbind(e1,e2),e3)

e11N<-mutate(e4,ID=1)


z.simpk<-function (DOSE){
  ## this is the event, notice the dose splitting 
  eN<-mutate(e11N, amt=c(0.5,0.5,DOSE/4,DOSE/4,DOSE/2,DOSE/2,DOSE,DOSE)*1000,rate=amt/0.167)
  df<-mod %>%
    drop.re %>%
    data_set(eN) %>%
    Req(CP,CLT,CL,CLNL) %>%
    mrgsim(end=40*7) %>%
    as.data.frame()
  df$GROUP<-paste0(DOSE," mg")
  return (df)
}

DOSE.level<-c(18,27,40,80,160,320)
LIST<-0

for (i in 1:length(DOSE.level)){
  DF<-z.simpk(DOSE.level[i])
  
  DF$ID<-i
  DF$DOSE<-DOSE.level[i]
  
  LIST[i]<-list(DF) 
}

pkData<-do.call("rbind",LIST)

pkData<-mutate(pkData,CONC=CP,CONC2=CP/1000, RO=CONC/(CONC+368),COHORT=factor(GROUP,levels = unique(GROUP)),
               GROUP=factor(GROUP,levels = unique(GROUP)),CLNL0=CLNL0,PNL=round(CLNL/CLNL0*100,3))

## get steady-state data
pk.ss<-subset(pkData, time>=33*7&time<=(35*7))
pk.Q1W<-subset(pkData, time>=11*7&time<=(13*7))

#### PK profile ########
QW<-seq(0,7*11,7)
QW2<-c(1,8,15,QW)
QW3<-QW2[order(QW2)]
Q2W<-seq(7*13,7*2*18, 14)
myDose<-data.frame(TIME=c(QW3,Q2W),y=20)

ggplot()+
  geom_line(data=pkData, aes(x = time, y = CONC2,color=COHORT),size=1) +
  geom_line(data=pkData, aes(x = time, y = CONC2,color=COHORT),size=1)+
  # geom_area(data=pk.ss, aes(x = time, y = CONC2,fill=COHORT),stat="identity",alpha=0.3,
  #           # position = position_stack(reverse = T),
  #           position = 'identity') +
  geom_area(data=pk.Q1W, aes(x = time, y = CONC2,fill=COHORT),stat="identity",alpha=0.3,
            # position = position_stack(reverse = T),
            position = 'identity') +
  geom_hline(yintercept=2000/1000,linetype="longdash")+
  geom_hline(yintercept=3660/1000,linetype="longdash")+
  # geom_segment(data=myDose,aes(x=TIME, y = -2000, xend = TIME, yend = 100),
  #              arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(data=myDose,aes(x=TIME, y = -5, xend = TIME, yend = 0.1),
               arrow = arrow(length = unit(0.2, "cm")))+
  # ylab("R1979 Concentration (mcg/L)")+
  ylab("R1979 Concentration (mg/L)")+
  xlab("Time (week)")+
  # coord_cartesian(ylim=c(0.0001,8000),xlim=c(0,77))+
  scale_x_continuous(breaks = seq(0,7*100,7),labels = seq(0,100,1))+
  # scale_y_continuous(breaks = seq(0,100000,10000))+
  scale_y_continuous(breaks = seq(0,100,10))+
  # coord_cartesian(ylim=c(-1000,32000))+
  po.nopanel








ggplot()+
  geom_line(data=pkData, aes(x = time, y = CONC,color=COHORT),size=1) +
  geom_line(data=pkData, aes(x = time, y = CONC,color=COHORT),size=1)+
  # geom_area(data=pk.ss, aes(x = time, y = CONC,fill=COHORT),stat="identity",alpha=0.3,
  #           # position = position_stack(reverse = T),
  #           position = 'identity') +
  geom_area(data=pk.Q1W, aes(x = time, y = CONC,fill=COHORT),stat="identity",alpha=0.3,
            # position = position_stack(reverse = T),
            position = 'identity') +
  # geom_hline(yintercept=2000,linetype="longdash")+
  # geom_hline(yintercept=3660,linetype="longdash")+
  geom_segment(data=myDose,aes(x=TIME, y = -2000, xend = TIME, yend = 100),
               arrow = arrow(length = unit(0.3, "cm")))+
  ylab("R1979 Concentration (mcg/L)")+
  xlab("Time (week)")+
  # coord_cartesian(ylim=c(0.0001,8000),xlim=c(0,77))+
  scale_x_continuous(breaks = seq(0,7*100,7),labels = seq(0,100,1))+
  scale_y_continuous(breaks = seq(0,100000,10000))+
  # coord_cartesian(ylim=c(-1000,32000))+
  po.nopanel




ggplot()+
  geom_line(data=pkData, aes(x = time, y = CLT,color=COHORT),size=1) +
  geom_line(data=pkData, aes(x = time, y = CLT,color=COHORT),size=1)+
  geom_segment(data=myDose,aes(x=TIME, y = 0.3, xend = TIME, yend = 0.5),
               arrow = arrow(length = unit(0.3, "cm")))+
  ylab("R1979 Concentration (mcg/L)")+
  xlab("Time (week)")+
  # coord_cartesian(ylim=c(0.0001,8000),xlim=c(0,77))+
  scale_x_continuous(breaks = seq(0,7*100,7),labels = seq(0,100,1))+
  # scale_y_continuous(breaks = seq(0,100000,10000))+
  coord_cartesian(ylim=c(0.25,1.5))+
  po.nopanel


ggplot()+
  geom_line(data=pkData, aes(x = time, y = CLNL,color=COHORT),size=1) +
  geom_line(data=pkData, aes(x = time, y = CLNL,color=COHORT),size=1)+
  geom_segment(data=myDose,aes(x=TIME, y = -0.2, xend = TIME, yend = 0),
               arrow = arrow(length = unit(0.3, "cm")))+
  ylab("Nonlinear clearance (L/day)")+
  xlab("Time (week)")+
  # coord_cartesian(ylim=c(0.0001,8000),xlim=c(0,77))+
  scale_x_continuous(breaks = seq(0,7*100,7),labels = seq(0,100,1))+
  # scale_y_continuous(breaks = seq(0,100000,10000))+
  coord_cartesian(ylim=c(0,0.3))+
  po.nopanel


### select Cmax

## target exposure reference
ref.df<-data.frame(CMAX=3660,CTROUGH=2000,CAVG=2500,AUC=17900)

## NOAEL exposure 
tox.df<-data.frame(CMAX=44200,CTROUGH=24400,CAVG=31430,AUC=220000)

## function to extract exposure and calculate fold

z.exp<-function (df,ii=14,ref=ref.df) {
  
  CMAX.df<-df %>% group_by(GROUP) %>% slice(which.max(CONC)) %>% 
    dplyr::select (ID, GROUP,DOSE,CONC) %>% mutate(NAME="CMAX",REF=ref$CMAX,FOLD=round(CONC/REF,2))%>%
    dplyr::rename(EXP = CONC)%>%  as.data.frame()
  
  CTROUGH.df<-df %>% group_by(GROUP) %>% filter(row_number()==1) %>% 
    dplyr::select (ID, GROUP,DOSE,CONC) %>%mutate(NAME="CTROUGH",REF=ref$CTROUGH,FOLD=round(CONC/REF,2))%>%
    dplyr::rename(EXP = CONC)%>%  as.data.frame()
  
  AUCss.df<-AUC(df,time='time',id='ID',dv='CONC')
  AUCss.df2<-AUCss.df%>%plyr::join(CMAX.df[,c("ID","GROUP","DOSE")],by="ID")%>%
    mutate(NAME="AUC",REF=ref$AUC,FOLD=round(AUC/REF,2))%>%dplyr::rename(EXP = AUC)%>%  as.data.frame()
  
  
  CAVG.df<-AUCss.df2%>% mutate(EXP=EXP/ii) %>% mutate(NAME="CAVG",REF=ref$CAVG,FOLD=round(EXP/REF,2))%>%
    as.data.frame()
  
  exp.df<-rbind(rbind(CMAX.df,CTROUGH.df),CAVG.df)
  
  exp.df$EXP<-round(exp.df$EXP,0)
  return (exp.df)
}

tmp1<-z.exp(pk.Q1W,ii=14,ref=tox.df)

tmp1<-mutate(tmp1,FOLD=round(1/FOLD,2))


tmp<-z.exp(pk.ss,ii=14,ref=ref.df)
# tmp<-mutate(tmp,FOLD=round(1/FOLD,1))


ggplot(tmp,aes(DOSE,EXP))+
  geom_point(shape=1,size=5,color="blue")+
  facet_wrap(~NAME)+
  xlab("Dose (mg)")+
  ylab("REGN1979 Concentration (mcg/L)")+
  geom_smooth(method = "lm", se = FALSE)+
  po.nopanel 


col<-c("GROUP","NAME","EXP","REF","FOLD")

write.table(tmp[,col], file="EXP_SS_320.csv",sep = ",",quote=FALSE,col.names = TRUE,row.names = F)

write.table(tmp1[,col], file="EXP_HIGHEST_margin_320.csv",sep = ",",quote=FALSE,col.names = TRUE,row.names = F)

