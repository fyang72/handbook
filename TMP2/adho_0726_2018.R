


# FDA questions on post-hoc clearance and volume of distribution 


adho



#----------------------------------------  
# adsl
#----------------------------------------
MODEL.HOME <- "H:\\FYANG\\R2810_PD1\\MODEL\\"
tdata = read.table(paste0(MODEL.HOME, "data/nmdat.csv"))  



#----------------------------------------  
# adsl
#----------------------------------------
MODEL.HOME <- "H:\\FYANG\\R2810_PD1\\MODEL\\"
adpx = read_OUTPUT(MODEL.HOME, subdir="ctl/HPC/", runno.lst="LN900", adpx00)  

adpx = adpx[["LN900"]] %>% filter(   C==".") #%>%
#distinct(USUBJID, .keep


tdata = adpx %>% distinct(USUBJID, .keep_all=TRUE)

tdata = tdata %>% mutate(V2 = as_numeric(V2), 
                         V3 = as_numeric(V3), 
                         CL = as_numeric(CL)
)

tdata %>% summarise(Vss_Mean = mean(V2 + V3), 
                    Vss_SD = sd(V2 + V3), 
                    Vss_CV = paste0(round(Vss_SD/Vss_Mean*100, digits=3),  "%"), 
                    
                    CL_Mean = mean(CL, na.rm=TRUE), 
                    CL_SD = sd(CL), 
                    CL_CV = paste0(round(CL_SD/CL_Mean*100, digits=3), "%")
) %>% as.data.frame() %>% transpose()

                    
                    
 



