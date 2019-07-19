  #----------------------------------------------------------------------------
  # Version 0.1   Created on 11/08/2018, Feng Yang
  # 
  #----------------------------------------------------------------------------
  # What are needed 
  # 1) a model file (.cpp)
  # 2) nmdat:  nonmem dataset; 
  
  # Goals: 
  # 1) Population: randomly sample the same population (nmdat) with the corresponding covariates, up to 1,000; 
  # 2) Use ideal dose regimens in nmdat,  
  # 3) Not use zero_re, randomly sampling the ETA parameter space. 
  #----------------------------------------------------------------------------
  library(mrgsolve)
  library(dplyr)
  library(dmutate)
  library(ggplot2)
  
  
  # Load model file
  #---------------------
  source("./cpp/mod_R3918_human_MM_final.cpp")
  mod <- mread("R2810", tempdir(), mymodel)     # ?mread , ?mcode   mod <- mcode("map", code)
  
  mod <- mod %>% param(COVARIATE=1, SCALING=0)
  
  
  # Load nmdat 
  #---------------------
  nmdat = read_csv("./data/nmdat_PKPD_1024_2018.csv", 
                   col_type=cols(.default=col_character()))    # read as character as defualt
  
  nmdat = nmdat %>% 
     mutate(TIME = as_numeric(TIME), 
            DVOR = as_numeric(DVOR),
            DV = as_numeric(DV), 
            AMT = as_numeric(AMT), 
            MDV = as.integer(MDV),
            
            C5BL = as_numeric(C5BL), 
            CH50HBL = as_numeric(CH50HBL), 
            WGTBL = as_numeric(WGTBL),
                  
            ARMA = gsub("_", " ", ARMA, fix=TRUE), 
            ARMA = ifelse(ARMA=="400 mg SC QW", "15 mg/kg IV QW*1 + 400 mg SC QW*4", ARMA), 
            ARMA = ordered(ARMA, levels = ARMA %>% unique())
    )
  
  # user input
  cov.lst = c("C5BL", "CH50HBL", "WGTBL")
  n.rep = 10
  population.size = 10  # virtual population size for each dose regimen
   
  #dosing.regimen = c("1 mg/kg IV",   "3 mg/kg IV",   "10 mg/kg IV",  "30 mg/kg IV",  
  #                   "300 mg SC",    "600 mg SC",   "15 mg/kg IV QW*1 + 400 mg SC QW*4")
 
  #or from nmat
  dosing.regimen = nmdat %>% pull(ARMA) %>% unique()
  
  #------------------------------------------------------------
  fig = empty_canvas(label="Cpp Model\n VPC")
  attr(fig, 'title') <-  ""
  FIGURE_ALL[["cppModel_VPC"]] = fig 
  
  #############################################################################
  #############################################################################
  # 1) Population (adsl): randomly sample the same population (nmdat) with the corresponding covariates, up to 1,000; 
  # 2) Dose (adex): Use ideal dose regimens in nmdat,  
  # 3) cpp model: COVARIATE=1, 
  #               zero_re off, i.e. randomly sampling the ETA parameter space. 
  #############################################################################
  #############################################################################
  #ID  TIME  DV AMT DOSE MDV ISM
  obs = nmdat  %>%
    filter(TEST==PK.TEST.NAME, MDV==0)  %>% 
    mutate(DVOR = ifelse(as_numeric(DVOR)==0, 0.078/2, as_numeric(DVOR)))
  
  
  
  #---------------------------------------------------------------------------------
  # Virtual population randomly sampled from the actual population with replacement
  # + ideal dose regimen (not the actual dose regimens) 
  # + actual time points   (otherwise it may take longer time)
  #---------------------------------------------------------------------------------

  # https://github.com/metrumresearchgroup/mrgsolve/issues/204
  
  # set.seed(1234)
  # adsl <- 
  #   data_frame(ID=1:100) %>% 
  #   mutate_random(WGT[50,110] ~ rnorm(80,30)) %>%   # [lower,upper] ~ rnorm(mu,sd))
  #   mutate_random(SEX ~ rbinomial(0.7)) %>%   # "1"=70%, "0"=30%
  #   mutate(dosegr = as.integer(WT > 90))
  # 
 
  
  # construct adsl based on nmdat, KEY: USUBJID
  adsl = nmdat %>% 
    distinct(USUBJID, .keep_all=TRUE)  %>% 
    select(USUBJID, ID, one_of(cov.lst)) %>%   # no ARMA
    mutate(POP = "VPC")
           
  # run population prediction
  sim0 = runSim_by_dosing_regimen(mod %>% zero_re(), 
                           adsl, 
                           dosing.regimen, 
                           delta = 100,       # default density of time point                           
                           tgrid = unique(as_numeric(nmdat$NTIM)),  # extra timepoint, from nmdat$NTIM
                           infusion.hour = 1,
                           followup.period = 84,
                           seed=1234)  %>% 
    as.data.frame() 
  
  # run individual simulation
  sim = lapply(1:n.rep, function(i) {
    
    print(i)
    
    # virtual subject 
    idata = adsl %>% 
      sample_n(size=population.size, replace=TRUE) %>% 
      mutate(ID = 1:population.size)   
  
    # run simulation   
    cbind(REP=i,
          runSim_by_dosing_regimen(mod, # %>% param(COVARIATE=1, SCALING=0), 
                                   idata, 
                                   dosing.regimen, 
                                   delta = 100,       # default density of time point                           
                                   tgrid = unique(as_numeric(nmdat$NTIM)),  # extra timepoint, from nmdat$NTIM
                                   infusion.hour = 1,
                                   followup.period = 84,
                                   seed=1234) %>% as.data.frame() 
    )
   }) %>% bind_rows()
  
  # combined both population and individual prediction
  sim = sim %>% mutate(IPRED_CONC = CENTRAL/V2, 
                       IPRED_CH50 = CH50) %>% 
    left_join(sim0 %>% 
                mutate(PRED_CONC = CENTRAL/V2, 
                       PRED_CH50 = CH50)%>% 
                
                select(ARMA, USUBJID, TIME, PRED_CONC, PRED_CH50), 
                          by = c("ARMA", "USUBJID", "TIME")) %>% 
    mutate(NTIM = TIME, 
           ARMA = ordered(ARMA, levels = ARMA %>% unique())
    )
  
  sim$ARMA %>% unique()
  head(sim)
  
   
  #############################################################################
  # Calcualte the VPC data
  #############################################################################
  # obs = simple_data$obs %>%  rename( ARMA = DOSE) %>% filter(MDV==0)  %>% 
  #   mutate(DVOR = DV) %>% #filter(TEST==PK.TEST.NAME) %>% 
  #   mutate(DVOR = ifelse(as_numeric(DVOR)==0, 0.078/2, DVOR))
  # sim = simple_data$sim %>%  rename( ARMA = DOSE) %>% filter(MDV==0)
  #
  # vpcdb = vpc(sim = simple_data$sim, obs = simple_data$obs,
  #             stratify="DOSE",  
  #             pred_corr = TRUE, 
  #             pi = c(0.025, 0.975), 
  #             ci = c(0.025, 0.975), 
  #             vpcdb = TRUE)
  # 
  #  
  
  # calculate the bin for TIME
  
  
  flag_calc_bin = 0
  if (flag_calc_bin) {   
    bins = "jenks"         # percentiles
    n_bins = "auto" 
    bin_mid = "mean" 
    
    bins <- auto_bin(sim%>%mutate(idv=TIME), bins, #  for 'TIME' variable
                    type="kmeans",   # "time, data, density, auto, percentiles",
                                     # "jenks", "kmeans", "pretty", "quantile", "hclust", "sd", "bclust", "fisher")
                    n_bins)
    
    labeled_bins = bins[1] == "percentiles"
    sim <- bin_data(sim, bins, "TIME", labeled = labeled_bins) %>% rename(BIN=bin)
  }else{
    sim = sim %>% mutate(BIN = NTIM) # BIN 
  }
  

  flag_pred_corr = 1
  if (flag_pred_corr) {
   #https://link.springer.com/content/pdf/10.1208%2Fs12248-011-9255-z.pdf
   # It is important to notice that the lower
   # bound is from the typical model prediction and not the
   # dependent variable which could be lower than the lower
   # boundary for the typical model prediction (e.g., due to  random effects). 
   
    sim = sim %>% mutate(DV = DV_CONC, 
                         IPRED = IPRED_CONC,
                         PRED = PRED_CONC)
   
    pred_corr_lower_bnd = 0
  
   # message("Performing prediction-correction on observed data...")
   sim <- sim %>% dplyr::group_by(ARMA, BIN) %>% dplyr::mutate(pred_bin = median(PRED))
   ids = sim$PRED != 0
   sim[ids,]$DV <- pred_corr_lower_bnd + 
     (sim[ids,]$DV - pred_corr_lower_bnd) * (sim[ids,]$pred_bin - pred_corr_lower_bnd) / (sim[ids,]$PRED - pred_corr_lower_bnd)
   
   sim
  }
   
  
  
  #############################################################################
  # Median with shaded area btw PCT2P5 and PCT97P5 
  #############################################################################
  sim = sim %>% mutate(DV = DV_CONC)
  # dplyr::summarise_at(vars(DV), funs(min, q2p5, q05, q10, q50, mean, q90, q95, q97p5, max))
   pi = c(0.025, 0.975)
   aggr_sim  = sim %>% group_by(ARMA, BIN) %>% dplyr::summarise(
     N = length(unique(USUBJID)), 
     q5 = quantile(DV, pi[1]), 
     q50 = quantile(DV, 0.5), 
     q95 = quantile(DV, pi[2]), 
     mTIME = mean(TIME)
   )
   
  fig = ggplot(data=aggr_sim, aes(x=mTIME, y=q50, group=ARMA)) +   
    geom_line() + 
    geom_ribbon(aes(ymin = q5, ymax = q95), alpha=0.3)  + 
     
    base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE)) + 
  
    xlab("Time (day)") + 
    ylab("Population Predicted/observed Concentration (mg/L)")  
    
  fig = fig + 
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)), labels=trans_format("log10", math_format(10^.x))) + 
    annotation_logticks(sides ="l")   
  
  
  
  # add observed data
  obs = obs %>% #filter(TEST==PK.TEST.NAME) %>% 
                  mutate(DVOR = ifelse(as_numeric(DVOR)==0, 0.078/2, DVOR)
  )
  
  fig = fig + facet_wrap(~ARMA, scales="free")  + 
    geom_point(data=obs, aes(x=TIME, y=DVOR), inherit.aes = FALSE) 
  
  fig
  
  attr(fig, 'title') <- "VPC of Predicted/observed (95%) Time Profiles"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["VPC1"]] = fig  
  
  

  
   
  #############################################################################
  # Median, PCT2P5 and PCT97P5 and their corresponding shaded areas 
  ############################################################################# 
  # lets do some additiona derivations, we can apply a bunch of
  # functions at a specific column using mutate_at,    summarise_at
  #That would further calculate the min, 5th percentile, median, mean, 95th percentile and max value for cmin
  # https://github.com/metrumresearchgroup/mrgsolve/issues/295
  
  # data
  #----------
  pi = c(0.025, 0.975)
  aggr_sim  = sim %>% group_by(REP, ARMA, BIN) %>% dplyr::summarise(
    q5 = quantile(DV, pi[1]), 
    q50 = quantile(DV, 0.5), 
    q95 = quantile(DV, pi[2]), 
    mTIME = mean(TIME)
  )
  
  ci = c(0.025, 0.975)
  vpc_dat  = aggr_sim %>% group_by(ARMA, BIN) %>% dplyr::summarise(
    q5.low = quantile(q5, ci[1]), 
    q5.med = quantile(q5, 0.5), 
    q5.up  = quantile(q5, ci[2]), 
    
    q50.low = quantile(q50, ci[1]), 
    q50.med = quantile(q50, 0.5), 
    q50.up  = quantile(q50, ci[2]), 
    
    q95.low = quantile(q95, ci[1]), 
    q95.med = quantile(q95, 0.5), 
    q95.up  = quantile(q95, ci[2]), 
    
    bin_mid = mean(mTIME)
  )
  
  vpc_dat$bin_min <- rep(bins[1:(length(bins)-1)], length(unique(vpc_dat$ARMA)))[vpc_dat$BIN]
  vpc_dat$bin_max <- rep(bins[2:length(bins)], length(unique(vpc_dat$ARMA)))[vpc_dat$BIN]
  
  
  # plot
  #----------
  fig = ggplot() +  #Mean, aes(x=TIME, y=Mean)) + geom_line() + 
    #q50
    geom_line(data=vpc_dat, aes(x=bin_mid, y=q50.med, group=ARMA)) + 
    geom_ribbon(data=vpc_dat, aes(x=bin_mid, ymin = q50.low, ymax = q50.up), alpha=0.3)  + 
    facet_wrap(~ARMA, scales="free")  + 
    
    # q5
    geom_line(data=vpc_dat, aes(x=bin_mid, y=q5.med)) + 
    geom_ribbon(data=vpc_dat, aes(x=bin_mid, ymin = q5.low, ymax = q5.up), alpha=0.3)  + 
    
    # q95
    geom_line(data=vpc_dat, aes(x=bin_mid, y=q95.med)) + 
    geom_ribbon(data=vpc_dat, aes(x=bin_mid, ymin = q95.low, ymax = q95.up), alpha=0.3)  + 
    
    theme_bw() + base_theme(font.size = as.integer(12)) + 
    guides(col=guide_legend(ncol=4,byrow=TRUE))  +  
    
    xlab("Time (day)") + 
    ylab("Population Predicted/observed Concentration (mg/L)")  
  
  fig = fig + 
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^round(x, digits=0)), labels=trans_format("log10", math_format(10^.x))) + 
    annotation_logticks(sides ="l") 
    
  fig = fig + facet_wrap(~ARMA, scales="free")  + 
    geom_point(data=obs, aes(x=TIME, y=DVOR), inherit.aes = FALSE) 
  
  fig
  
  
  attr(fig, 'title') <- "VPC of Predicted/observed (95%) Time Profiles"
  attr(fig, 'width') <- 8
  attr(fig, 'height') <- 6
  FIGURE_ALL[["VPC2"]] = fig  
  
  fig
  
 
  
  # http://www.pmxsolutions.com/2018/09/21/a-step-by-step-guide-to-prediction-corrected-visual-predictive-checks-vpc-of-nonmem-models/
  # Prediction corrected visual predictive check .Black dots = prediction corrected observations, 
  # black dashed lines = 80%-confidence interval (CI) and median of the prediction corrected observations,
  # red shaded area = 95%-CI of the median prediction, 
  # blue shaded area = 95%-CI of the 10 and 80th prediction percentiles.
  
  
  
  
   