

######################################################################
# setup
######################################################################
 
library(stringr)
HOME = paste0(normalizePath("."), "/")
if (str_sub(HOME, 1, 22) == "/home/feng.yang/FYANG/") {HOME=paste0("/home/feng.yang/YANG/")}
if (str_sub(HOME, 1, 19) == "C:\\Users\\feng.yang\\") {HOME=paste0("C:\\Users\\feng.yang\\Documents\\handbook/")}


#source(paste0(HOME, "global.R"))
library(dplyr)
library(mrgsolve)
library(ggplot2)
library(xpose4)
library(dmutate)
library(gdata)
library(stringr)
library(officer)   
library(PKPDmisc)
library(RColorBrewer) 
library(shiny)

actionButton_style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
options(bitmapType='cairo')   # solve Warning: Error in grDevices::png: X11 is not available

list_files_in_a_folder <- function(folder.loc="./util/", file.extension=c(".r", ".R")) {
  file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, include.dirs = TRUE, recursive =TRUE)     
  file.lst = file.lst[which(substr(file.lst, nchar(file.lst)-(nchar(file.extension)-1), nchar(file.lst)) %in% file.extension)]
  file.lst = file.lst[which(!substr(basename(file.lst), 1, 1) %in% "_")]
  file.lst = file.lst[setdiff(1:length(file.lst), grep("_not_used", file.lst, fixed=TRUE))]
  return(file.lst)
}

ihandbook = 0

file.lst <- list_files_in_a_folder(folder.loc=paste0(HOME, "/util/"), file.extension=c(".r", ".R"))
for (ifile in 1:length(file.lst)) { print(file.lst[ifile]); source(file=file.lst[ifile]) }     

file.lst <- list_files_in_a_folder(folder.loc=paste0(HOME, "/script/"), file.extension=c(".r", ".R"))
for (ifile in 1:length(file.lst)) {source(file=file.lst[ifile])  }     


#---------------------------------------- 
# load mrgsolve cpp model
#---------------------------------------- 
cppModel_file <- paste0(HOME, "/cpp/LN001.cpp")
cppModel <- read_cppModel(cppModel_file, "TEST")  
 
# output results
FIGURE  = NULL
TABLE = NULL


set.seed(1234) 


#----------------------------------------  
# dose regimens to be simulated
#----------------------------------------
 
 
adex = parseARMA(c("350 mg IV Q3W*6",
                   "438 mg SC Q3W*1 + 350 mg IV Q3W*5")) 

#----------------------------------------  
# define adsl
#---------------------------------------- 
nrep = 1   # n replicate
nsubject = 1  # define a population

# define population
adsl_1 <- 
  data_frame(USUBJID=1:nsubject) %>% 
  #mutate_random(WGTBL[50,110] ~ rnorm(80,30)) %>%   # [lower,upper] ~ rnorm(mu,sd))
  mutate_random(WGTBL[50,110] ~ rnorm(70,0)) %>%   # [lower,upper] ~ rnorm(mu,sd))
  mutate_random(SEX ~ rbinomial(0.7))     # "1"=70%, "0"=30%
   

nrep = 1   # n replicate
nsubject = 100  # define a population

# define population
adsl_100 <- 
  data_frame(USUBJID=1:nsubject) %>% 
  #mutate_random(WGTBL[50,110] ~ rnorm(80,30)) %>%   # [lower,upper] ~ rnorm(mu,sd))
  mutate_random(WGTBL[50,110] ~ rnorm(70,0)) %>%   # [lower,upper] ~ rnorm(mu,sd))
  mutate_random(SEX ~ rbinomial(0.7))     # "1"=70%, "0"=30%

#adsl = data.frame(USUBJID=1, WGTBL=75)  # a data.frame
 
#---------------------------------------- 
# run a single simulation
#---------------------------------------- 

seed = 1234
simulation_delta = 1
followup_period = 112  # day
infusion_hrs_lst = 1 

simData_1 <- runSim_by_dosing_regimen(
  cppModel %>% zero_re,    # model file
  adsl_1,   # population 
  adex,   # dose regimen
  simulation_delta = simulation_delta,  # integration step                  
  tgrid = NULL, # c(0.00001,0.001, 0.01, 0.1),     # extra timepoint (other than delta)
  infusion_hrs_lst = infusion_hrs_lst,  # hour,  infusion hours
  followup_period = followup_period,   # how long of the followup period after treatment
  seed=seed)

simData_100 <- runSim_by_dosing_regimen(
  cppModel,    # model file
  adsl_100,   # population 
  adex,   # dose regimen
  simulation_delta = simulation_delta,  # integration step                  
  tgrid = NULL, # c(0.00001,0.001, 0.01, 0.1),     # extra timepoint (other than delta)
  infusion_hrs_lst = infusion_hrs_lst,  # hour,  infusion hours
  followup_period = followup_period,   # how long of the followup period after treatment
  seed=seed)

#-----------------------------------------------
# run trial simulation (with replicate = nrep)
#-----------------------------------------------
nrep=0
simData = lapply(1:nrep, function(i) {
  
  print(i)
  
  # virtual subject 
  i_adsl = adsl_100 %>% 
    sample_n(size=nsubject, replace=TRUE) %>% 
    mutate(USUBJID = 1:nsubject)   
  
  # run simulation   
  cbind(REP=i,
        runSim_by_dosing_regimen(cppModel, 
                                 i_adsl, 
                                 adex, 
                                 simulation_delta = simulation_delta,       # default density of time point                           
                                 tgrid = NULL,  # extra timepoint, from nmdat$TIME
                                 infusion_hrs_lst = infusion_hrs_lst,
                                 followup_period = followup_period,
                                 seed=1234) %>% as.data.frame() 
  )
}) %>% bind_rows()
 
#---------------------------------------- 
# priminary plot     
# -----------------------------------
tdata = simData_1 %>% 
  mutate(TIME = TIME/7, 
         DVOR = CENTRAL/V2
         )
 
STUDY.NAME = "tmp"
PK.TEST.NAME = "TEST"
PK.TEST.LABEL = "TEST"
x=setup_scale(myscale='1_4', mylimit=c(0, max(tdata$TIME, na.rm=TRUE)))

fig = ggplot(tdata, aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + 
  #facet_wrap(~POP) +
  #ggtitle("Concentration Time Profile") + 
  
  #geom_point(aes(shape=ARMA, size=ARMA)) +  
  geom_line() +   
  
  scale_color_manual(values=colScheme()) + 
  scale_shape_manual(values=shapeScheme())+
  scale_size_manual(values=rep(3, times=length(shapeScheme()))) +
  
  scale_x_continuous(breaks=x$breaks, label=x$labels) +
  #scale_x_continuous(trans=‘log2’), scale_y_continuous(trans=‘log2’) : another allowed value for the argument trans is ‘log10’
  
  scale_y_log10(breaks = 10^(seq(-3,3,by=1)), #trans_breaks("log10", function(x) 10^x),
                labels = 10^(seq(-3,3,by=1))) + # trans_format("log10", math_format(10^.x))) +
  
  #coord_trans( y="log10") +   # possible values for x and y are “log2”, “log10”, “sqrt”, …
  
  #coord_cartesian(xlim = c(0, 6)) + 
  
  xlab("Time (week)") + 
  ylab(paste0("Concentration of ", PK.TEST.LABEL, " (mg/L)")) +   
  
  theme_bw() + base_theme(font.size = as.integer(12)) + 
  guides(col=guide_legend(ncol=4,byrow=TRUE)) 

# individual log scale 
hline <- c(0.078)
hline_location = c(3)  #eval(parse(text=paste0("c(", values$hline_location, ")")))
hline_label <- paste0(hline, " mg/L")

fig = fig + annotation_logticks(sides ="l")  +   # "trbl", for top, right, bottom, and left.
  geom_hline(yintercept = hline, lty="dashed") + 
  annotate("text", hline_location, hline,vjust = -1,  label =  hline_label) 

attr(fig, 'title') <- paste0("Individual Log-scaled Concentration of ", PK.TEST.NAME, " in Serum vs Actual Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
FIGURE[["pk_indiv_log"]] = fig

# individual log scale by panel
fig = fig + facet_wrap(~ARMA) + ggplot2::theme(legend.position = "none")  
attr(fig, 'title') <- paste0("Individual Log-scaled Concentration of ", PK.TEST.NAME, " in Serum vs Actual Sampling Day Following Subcutaneous or Intravenous Dose(s) of ", PK.TEST.NAME, " (", STUDY.NAME, ")")
FIGURE[["pk_indiv_log_by_panel"]] = fig 
 




#-----------------------------------------------
# calcualte individual exposure metrics
#-----------------------------------------------
tdata = simData_1  %>% mutate(TIME = TIME, 
                         DVOR = CENTRAL/V2)

group_by = c("ARMA", "USUBJID", "TEST")

# filter out the last dosing interval
tdata = tdata  %>%  
  group_by_(.dots =c(group_by )) %>%  
  top_n(n=1, wt=EXSEQ) %>% filter(TAD<=II)


# calculate the PK parameter for each subject and each time interval
out = tdata  %>% 
  group_by_(.dots =c(group_by )) %>%   
  dplyr::summarise(
    #CMIN = round(min(DVOR),digits=3),    # Note the infusion hours, could be 5 min to 2 hr, typically.
    CMIN = round(DVOR[n()],digits=3),   # the last conc at this time interval
    CMAX = round(max(DVOR),digits=3),       
    AUCtau = round(auc_partial(TIME, DVOR),digits=3)    # , data%>%pull(input$yvar))   #, range=data%>%range_(input$xvar)
  )  

out = out%>% ungroup()  %>%  select(-TEST) %>% 
  gather(key=TEST, value=DVOR, -one_of(setdiff(group_by, "TEST")))

tabl = out  %>% calc_stats(id="USUBJID", group_by=c("ARMA", "TEST"), value="DVOR")  %>% 
       select(ARMA, TEST, N, Mean_SD)  %>% 
       spread(TEST, Mean_SD)
 
 
TABLE[["summary_stats_exposure_metrics"]]  = tabl


  


#-----------------------------------------------
# calcualte 95% CI interval 
#-----------------------------------------------

tdata =  simData_100 %>% 
  mutate(TIME=TIME/7, DVOR=IPRED) %>% 
  calc_stats(id="USUBJID", group_by=c("ARMA", "TIME"), value="DVOR") %>% 
  select(ARMA, TIME, N, Mean_CV, SE, SD, Median, PCT2P5, PCT97P5, GEOmean, GEOSD) 
  

x=setup_scale(myscale='1_4', mylimit=c(0, max(tdata$TIME, na.rm=TRUE)))

fig = ggplot(tdata, aes(x=TIME, y=Median, group=ARMA)) + 
  geom_line(col="black", show.legend=TRUE  ) +
  geom_ribbon(aes(ymin=PCT2P5,ymax=PCT97P5), fill="gray50", alpha="0.5", show.legend=F)   + 
  #scale_fill_manual(values=c(clear,blue)) + 
  
  #coord_cartesian(xlim =xlim, ylim =ylim) + 
  
  xlab("Time (week)") + 
  ylab(paste0("Concentration of ", PK.TEST.LABEL, " (mg/L)")) + 
  
  theme_regn(font_size=12) + 
  scale_x_continuous(breaks=x$breaks, label=x$labels) + 
  facet_wrap(~ARMA)
  
  
attr(fig, 'title') <- paste0("Predeicted Time-profile With 95% Confidence Interval")
FIGURE[["pk_time_profile_95_CI"]] = fig 

fig



#-----------------------------------------------
# final output to doc and ppt
#-----------------------------------------------
library(officer)

mydocx <- read_docx(paste0(HOME, "/lib/docTemplate.docx")) %>% 
  print2docx(FIGURE, TABLE) 
print(mydocx, target = './docx_result.docx')

mypptx <- read_pptx(paste0(HOME, "/lib/pptTemplate.pptx")) %>% 
  print2pptx(FIGURE, TABLE) 
print(mypptx, target = './pptx_result.pptx')
