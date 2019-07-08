

parsePCTPT <- function(PCTPT, DAY.IN.CYCLE=56, INFHR=1) {
  
  PCTPT.TEST = c( "DAY 1",
                  "DAY -1", 
                  "DAY-1", 
                  "POST",                
                  "PRE",               
                  "Hour 1", 
                  "Hour 2",
                  "DAY 1 HOUR 2",
                  "POST-DOSE: 0.25 HOUR",
                  "POST-DOSE: 0.5 HOUR", 
                  "POST-DOSE: 1 HOUR" ,  
                  "POST-DOSE: 2 HOURS",  
                  "POST-DOSE: 4 HOURS",  
                  "POST-DOSE: 8 HOURS" , 
                  "POST-DOSE: 12 HOURS" ,
                  "D2",                  
                  "D4" ,                 
                  "D8" ,                 
                  "D15" ,                
                  "D22",                 
                  "D29" ,                
                  "D43" ,                
                  "D57",                 
                  "D85",                 
                  "D99",               
                  "D113" ,               
                  "D-1"  ,               
                  "D -1", 
                  "D1 PRE",
                  "D1 0HR",
                  "D1 15MIN",
                  "D1 30MIN",
                  "D1 1HR",
                  "D1 2HR",
                  "D1 4HR",
                  "D1 8HR",
                  "D1 12HR",
                  "D2", "D4", "D8", "D15", "D22", "D29")
  
  # https://www.regular-expressions.info/numericranges.html
  PCTPT = PCTPT[!is.na(PCTPT) & PCTPT!=""]
  PCTPT.ORG = PCTPT %>% unique()
  PCTPT = toupper(PCTPT) %>% unique()
  
  # one by one
  # expr1 = '((D|DAY)(-|\\s+|)[0-9]+)'  # DAY-1
  # expr2 = '((D|DAY)(\\s+)(-)[0-9]+)'  # DAY -1
  # DAY = PCTPT %>% extractExpr(regexpr=paste(expr1, expr2,sep="|"))
  # 
  # expr1 = '(\\(?[0-9,.]+)'  # DAY-1
  # expr2 = '(\\(?(\\s+)(-)[0-9,.]+)'  # DAY -1
  # DAY = DAY %>% extractExpr(regexpr=paste(expr1, expr2,sep="|")) %>% as.numeric()
  # 
  num_expr <- "[-+]?([0-9]*\\.?[0-9]+)"  # any positive/negative decimal number
  
  # https://stackoverflow.com/questions/2811031/decimal-or-numeric-values-in-regular-expression-validation
  # https://stackoverflow.com/questions/5917082/regular-expression-to-match-numbers-with-or-without-commas-and-decimals-in-text
  #https://www.regular-expressions.info/floatingpoint.html
  DAY_expr <- "(D|DAY)(-|\\s+|)"  
  DAY = PCTPT %>% extractExpr(regexpr=paste0(DAY_expr, num_expr))%>%   
    extractExpr(regexpr=num_expr)  %>% as.numeric()  
  PCTPT <- PCTPT %>% removeExpr(regexpr=paste0(DAY_expr, num_expr))
  
  
  #PCTPT = gsub("PREINFUSION", "PRE", PCTPT, fixed=TRUE)
  #PCTPT = gsub("END OF INFUSION", "POST", PCTPT, fixed=TRUE)
  PRE_expr = '(\\s+|)(PRE|PREINFUSION)+'
  PRE = extractExpr(PCTPT, regexpr=PRE_expr)  %>% as.vector()  %>% trim()
  PCTPT <- PCTPT %>% removeExpr(regexpr=PRE_expr)
  
  POST_expr <- '(\\s+|)(0HR|POST|END OF INFUSION)+'
  POST = extractExpr(PCTPT, regexpr=POST_expr)    %>% as.vector()  %>% trim()
  PCTPT <- PCTPT %>% removeExpr(regexpr=POST_expr)
  
  option1 = paste0("(", '(HR|HOUR|HOURS)(-|\\s+|)', num_expr, ")")
  option2 = paste0("(", num_expr, '(-|\\s+|)(HR|HOUR|HOURS)', ")")
  HOUR_expr = paste(option1, "|", option2, sep="", collapse="")
  HOUR <- extractExpr(PCTPT, regexpr=HOUR_expr)  %>%  extractExpr(regexpr=num_expr) %>% as.numeric()
  PCTPT <- PCTPT %>% removeExpr(regexpr=HOUR_expr)
  
  MIN_expr <- paste0(num_expr, '(-|\\s+|)(MIN|MINUTE|MINUTES)')
  MIN = extractExpr(PCTPT, regexpr=MIN_expr)  %>%   extractExpr(regexpr=num_expr) %>% as.numeric()
  PCTPT <- PCTPT %>% removeExpr(regexpr=MIN_expr)
  
  WEEK_expr <- paste0('(WEEK|WK)(-|\\s+|)', num_expr)
  WEEK = extractExpr(PCTPT, regexpr=WEEK_expr)  %>%  extractExpr(regexpr=num_expr) %>% as.numeric()
  PCTPT <- PCTPT %>% removeExpr(regexpr=WEEK_expr)
  
  CYCLE_expr <- paste0('(CYCLE)(-|\\s+|)', num_expr)
  CYCLE = extractExpr(PCTPT, regexpr=CYCLE_expr)  %>%   extractExpr(regexpr=num_expr) %>% as.numeric()
  PCTPT <- PCTPT %>% removeExpr(regexpr=CYCLE_expr)
  
  VISIT_expr <- paste0('(VISIT)(-|\\s+|)', num_expr)
  VISIT = extractExpr(PCTPT, regexpr=VISIT_expr)  %>%  extractExpr(regexpr=num_expr) %>% as.numeric()
  PCTPT <- PCTPT %>% removeExpr(regexpr=VISIT_expr)
  PCTPT
  
  OUT = bind_cols(PCTPT=PCTPT.ORG, CYCLE=CYCLE, WEEK=WEEK,VISIT=VISIT, DAY=DAY, HOUR=HOUR, MIN=MIN, PRE=PRE, POST=POST) %>% as.data.frame()
  
  # convert clinical Day 1 as NTIM=0, and so on.
  OUT  = OUT %>% mutate(CYCLE=ifelse(is.na(CYCLE), 1, CYCLE), 
                        WEEK=ifelse(is.na(WEEK), 0, WEEK), 
                        VISIT=ifelse(is.na(VISIT), 1, VISIT),
                        
                        DAY=ifelse(is.na(DAY), 1, DAY), 
                        DAY=ifelse(DAY>0, DAY-1, DAY),
                        
                        HOUR=ifelse(is.na(HOUR), 0, HOUR), 
                        MIN=ifelse(is.na(MIN), 0, MIN), 
                        
                        NTIM = (CYCLE-1)*DAY.IN.CYCLE + (WEEK)*7 + DAY + HOUR/24 + MIN/60/24)
  
  #to be discussed later
  # if PCTPT recorded from the end of infusion, 
  #INFHR = 1 # assume 1 hour infusion if IV dose
  #OUT = OUT %>% mutate(NTIM=ifelse((HOUR>0&HOUR<=24) | (MIN>0&MIN<60) | POST!=0, NTIM+INFHR/24, NTIM))  #NTIM<1, NTIM+INFHR/24, NTIM))
  
  key.lst = c("PCTPT", "NTIM")
  OUT = OUT[, c(key.lst, setdiff(colnames(OUT), key.lst))]
  
  # clean up 
  if(all(OUT$CYCLE==1)) {OUT = OUT %>% select(-CYCLE)}
  if(all(OUT$WEEK==1)) {OUT = OUT %>% select(-WEEK)}
  if(all(OUT$VISIT==1)) {OUT = OUT %>% select(-VISIT)}  
  
  if(all(OUT$DAY==0)) {OUT = OUT %>% select(-DAY)}
  if(all(OUT$HOUR==0)) {OUT = OUT %>% select(-HOUR)}
  if(all(OUT$MIN==0)) {OUT = OUT %>% select(-MIN)}
  
  if(all(is.na(OUT$PRE))) {OUT = OUT %>% select(-PRE)}
  if(all(is.na(OUT$POST))) {OUT = OUT %>% select(-POST)}
  
  
  return(OUT)
}








parseTIMEPT <- function(TIMEPT, DAY.IN.CYCLE=56, INFHR=1) {
  
  TIMEPT.TEST = c( "DAY 1",
                   "DAY -1", 
                   "DAY-1", 
                   "POST",                
                   "PRE",               
                   "Hour 1", 
                   "Hour 2",
                   "DAY 1 HOUR 2",
                   "POST-DOSE: 0.25 HOUR",
                   "POST-DOSE: 0.5 HOUR", 
                   "POST-DOSE: 1 HOUR" ,  
                   "POST-DOSE: 2 HOURS",  
                   "POST-DOSE: 4 HOURS",  
                   "POST-DOSE: 8 HOURS" , 
                   "POST-DOSE: 12 HOURS" ,
                   "D2",                  
                   "D4" ,                 
                   "D8" ,                 
                   "D15" ,                
                   "D22",                 
                   "D29" ,                
                   "D43" ,                
                   "D57",                 
                   "D85",                 
                   "D99",               
                   "D113" ,               
                   "D-1"  ,               
                   "D -1", 
                   "D1 PRE",
                   "D1 0HR",
                   "D1 15MIN",
                   "D1 30MIN",
                   "D1 1HR",
                   "D1 2HR",
                   "D1 4HR",
                   "D1 8HR",
                   "D1 12HR",
                   "D2", "D4", "D8", "D15", "D22", "D29")
  
  # https://www.regular-expressions.info/numericranges.html
  TIMEPT = TIMEPT[!is.na(TIMEPT) & TIMEPT!=""]
  TIMEPT.ORG = TIMEPT %>% unique()
  TIMEPT = toupper(TIMEPT) %>% unique()
  
  # one by one
  # expr1 = '((D|DAY)(-|\\s+|)[0-9]+)'  # DAY-1
  # expr2 = '((D|DAY)(\\s+)(-)[0-9]+)'  # DAY -1
  # DAY = TIMEPT %>% extractExpr(regexpr=paste(expr1, expr2,sep="|"))
  # 
  # expr1 = '(\\(?[0-9,.]+)'  # DAY-1
  # expr2 = '(\\(?(\\s+)(-)[0-9,.]+)'  # DAY -1
  # DAY = DAY %>% extractExpr(regexpr=paste(expr1, expr2,sep="|")) %>% as.numeric()
  # 
  num_expr <- "[-+]?([0-9]*\\.?[0-9]+)"  # any positive/negative decimal number
  
  # https://stackoverflow.com/questions/2811031/decimal-or-numeric-values-in-regular-expression-validation
  # https://stackoverflow.com/questions/5917082/regular-expression-to-match-numbers-with-or-without-commas-and-decimals-in-text
  #https://www.regular-expressions.info/floatingpoint.html
  DAY_expr <- "(D|DAY)(-|\\s+|)"  
  DAY = TIMEPT %>% extractExpr(regexpr=paste0(DAY_expr, num_expr))%>%   
    extractExpr(regexpr=num_expr)  %>% as.numeric()  
  TIMEPT <- TIMEPT %>% removeExpr(regexpr=paste0(DAY_expr, num_expr))
  
  
  #TIMEPT = gsub("PREINFUSION", "PRE", TIMEPT, fixed=TRUE)
  #TIMEPT = gsub("END OF INFUSION", "POST", TIMEPT, fixed=TRUE)
  PRE_expr = '(\\s+|)(PRE|PREINFUSION)+'
  PRE = extractExpr(TIMEPT, regexpr=PRE_expr)  %>% as.vector()  %>% trim()
  TIMEPT <- TIMEPT %>% removeExpr(regexpr=PRE_expr)
  
  POST_expr <- '(\\s+|)(0HR|POST|END OF INFUSION)+'
  POST = extractExpr(TIMEPT, regexpr=POST_expr)    %>% as.vector()  %>% trim()
  TIMEPT <- TIMEPT %>% removeExpr(regexpr=POST_expr)
  
  option1 = paste0("(", '(HR|HOUR|HOURS)(-|\\s+|)', num_expr, ")")
  option2 = paste0("(", num_expr, '(-|\\s+|)(HR|HOUR|HOURS)', ")")
  HOUR_expr = paste(option1, "|", option2, sep="", collapse="")
  HOUR <- extractExpr(TIMEPT, regexpr=HOUR_expr)  %>%  extractExpr(regexpr=num_expr) %>% as.numeric()
  TIMEPT <- TIMEPT %>% removeExpr(regexpr=HOUR_expr)
  
  MIN_expr <- paste0(num_expr, '(-|\\s+|)(MIN|MINUTE|MINUTES)')
  MIN = extractExpr(TIMEPT, regexpr=MIN_expr)  %>%   extractExpr(regexpr=num_expr) %>% as.numeric()
  TIMEPT <- TIMEPT %>% removeExpr(regexpr=MIN_expr)
  
  WEEK_expr <- paste0('(WEEK|WK)(-|\\s+|)', num_expr)
  WEEK = extractExpr(TIMEPT, regexpr=WEEK_expr)  %>%  extractExpr(regexpr=num_expr) %>% as.numeric()
  TIMEPT <- TIMEPT %>% removeExpr(regexpr=WEEK_expr)
  
  CYCLE_expr <- paste0('(CYCLE)(-|\\s+|)', num_expr)
  CYCLE = extractExpr(TIMEPT, regexpr=CYCLE_expr)  %>%   extractExpr(regexpr=num_expr) %>% as.numeric()
  TIMEPT <- TIMEPT %>% removeExpr(regexpr=CYCLE_expr)
  
  VISIT_expr <- paste0('(VISIT)(-|\\s+|)', num_expr)
  VISIT = extractExpr(TIMEPT, regexpr=VISIT_expr)  %>%  extractExpr(regexpr=num_expr) %>% as.numeric()
  TIMEPT <- TIMEPT %>% removeExpr(regexpr=VISIT_expr)
  TIMEPT
  
  OUT = bind_cols(TIMEPT=TIMEPT.ORG, CYCLE=CYCLE, WEEK=WEEK,VISIT=VISIT, DAY=DAY, HOUR=HOUR, MIN=MIN, PRE=PRE, POST=POST) %>% as.data.frame()
  
  # convert clinical Day 1 as NTIM=0, and so on.
  OUT  = OUT %>% mutate(CYCLE=ifelse(is.na(CYCLE), 1, CYCLE), 
                        WEEK=ifelse(is.na(WEEK), 0, WEEK), 
                        VISIT=ifelse(is.na(VISIT), 1, VISIT),
                        
                        DAY=ifelse(is.na(DAY), 1, DAY), 
                        DAY=ifelse(DAY>0, DAY-1, DAY),
                        
                        HOUR=ifelse(is.na(HOUR), 0, HOUR), 
                        MIN=ifelse(is.na(MIN), 0, MIN), 
                        
                        NTIM = (CYCLE-1)*DAY.IN.CYCLE + (WEEK)*7 + DAY + HOUR/24 + MIN/60/24)
  
  #to be discussed later
  # if TIMEPT recorded from the end of infusion, 
  #INFHR = 1 # assume 1 hour infusion if IV dose
  #OUT = OUT %>% mutate(NTIM=ifelse((HOUR>0&HOUR<=24) | (MIN>0&MIN<60) | POST!=0, NTIM+INFHR/24, NTIM))  #NTIM<1, NTIM+INFHR/24, NTIM))
  
  key.lst = c("TIMEPT", "NTIM")
  OUT = OUT[, c(key.lst, setdiff(colnames(OUT), key.lst))]
  
  # clean up 
  if(all(OUT$CYCLE==1)) {OUT = OUT %>% select(-CYCLE)}
  if(all(OUT$WEEK==1)) {OUT = OUT %>% select(-WEEK)}
  if(all(OUT$VISIT==1)) {OUT = OUT %>% select(-VISIT)}  
  
  if(all(OUT$DAY==0)) {OUT = OUT %>% select(-DAY)}
  if(all(OUT$HOUR==0)) {OUT = OUT %>% select(-HOUR)}
  if(all(OUT$MIN==0)) {OUT = OUT %>% select(-MIN)}
  
  if(all(is.na(OUT$PRE))) {OUT = OUT %>% select(-PRE)}
  if(all(is.na(OUT$POST))) {OUT = OUT %>% select(-POST)}
  
  
  return(OUT)
}




parseTIMEPT_OLD <- function(TIMEPT, DAY.IN.CYCLE=56, INFHR=1) {
  TIMEPT_TEST = c( "POST",                
                   "PRE",                 
                   "POST-DOSE: 0.25 HOUR",
                   "POST-DOSE: 0.5 HOUR", 
                   "POST-DOSE: 1 HOUR" ,  
                   "POST-DOSE: 2 HOURS",  
                   "POST-DOSE: 4 HOURS",  
                   "POST-DOSE: 8 HOURS" , 
                   "POST-DOSE: 12 HOURS" ,
                   "D2",                  
                   "D4" ,                 
                   "D8" ,                 
                   "D15" ,                
                   "D22",                 
                   "D29" ,                
                   "D43" ,                
                   "D57",                 
                   "D85",                 
                   "D99",               
                   "D113" ,               
                   "D-1"  ,               
                   "D -1", 
                   "D1 PRE",
                   "D1 0HR",
                   "D1 15MIN",
                   "D1 30MIN",
                   "D1 1HR",
                   "D1 2HR",
                   "D1 4HR",
                   "D1 8HR",
                   "D1 12HR",
                   "D2", "D4", "D8", "D15", "D22", "D29")
  
  
  # https://www.regular-expressions.info/numericranges.html
  TIMEPT = TIMEPT[!is.na(TIMEPT) & TIMEPT!=""]
  TIMEPT.ORG = TIMEPT %>% unique()
  TIMEPT = toupper(TIMEPT) %>% unique()
  
  # one by one
  expr1 = '((D|DAY)(-|\\s+|)[0-9]+)'  # DAY-1
  expr2 = '((D|DAY)(\\s+)(-)[0-9]+)'  # DAY -1
  DAY = TIMEPT %>% extractExpr(regexpr=paste(expr1, expr2,sep="|"))
  
  

  
  
  
  
  expr1 = '(\\(?[0-9,.]+)'  # DAY-1
  expr2 = '(\\(?(\\s+)(-)[0-9,.]+)'  # DAY -1
  DAY = DAY %>% extractExpr(regexpr=paste(expr1, expr2,sep="|")) %>% as.numeric()
  
  PRE = extractExpr(TIMEPT, regexpr='(\\s+|)(PRE)+')  %>% as.vector()  %>% trim()
  
  POST = extractExpr(TIMEPT, regexpr='(\\s+|)(0HR|POST)+')    %>% as.vector()  %>% trim()
  
  HR = extractExpr(TIMEPT, regexpr='[0-9]+(-|\\s+|)(HR|HOUR)')  %>% extractExpr(regexpr='\\(?[0-9,.]+') %>% as.numeric()
  
  MIN = extractExpr(TIMEPT, regexpr='[0-9]+(-|\\s+|)(MIN|MINUTE|MINUTES)')  %>% extractExpr(regexpr='\\(?[0-9,.]+') %>% as.numeric()
  
  WEEK = extractExpr(TIMEPT, regexpr='(WEEK|WK)(-|\\s+|)[0-9]+')  %>% extractExpr(regexpr='\\(?[0-9,.]+') %>% as.numeric()
  
  CYCLE = extractExpr(TIMEPT, regexpr='(CYCLE)(-|\\s+|)[0-9]+')  %>% extractExpr(regexpr='\\(?[0-9,.]+') %>% as.numeric()
  
  VISIT = extractExpr(TIMEPT, regexpr='(VISIT)(-|\\s+|)[0-9]+')  %>% extractExpr(regexpr='\\(?[0-9,.]+') %>% as.numeric()
  
  OUT = bind_cols(TIMEPT=TIMEPT.ORG, CYCLE=CYCLE, WEEK=WEEK,VISIT=VISIT, DAY=DAY, HR=HR, MIN=MIN, PRE=PRE, POST=POST) %>% as.data.frame()
  
  
  # convert clinical Day 1 as NTIM=0, and so on.
  OUT  = OUT %>% mutate(CYCLE=ifelse(is.na(CYCLE), 1, CYCLE), 
                        WEEK=ifelse(is.na(WEEK), 1, WEEK), 
                        VISIT=ifelse(is.na(VISIT), 1, VISIT),
                        
                        DAY=ifelse(is.na(DAY), 1, DAY), 
                        DAY=ifelse(DAY>0, DAY-1, DAY),
                        
                        HR=ifelse(is.na(HR), 0, HR), 
                        MIN=ifelse(is.na(MIN), 0, MIN), 
                        
                        NTIM = (CYCLE-1)*DAY.IN.CYCLE + (WEEK-1)*7 + DAY + HR/24 + MIN/60/24)
  
  #to be discussed later
  # if TIMEPT recorded from the end of infusion, 
  #INFHR = 1 # assume 1 hour infusion if IV dose
  #OUT = OUT %>% mutate(NTIM=ifelse((HR>0&HR<=24) | (MIN>0&MIN<60) | POST!=0, NTIM+INFHR/24, NTIM))  #NTIM<1, NTIM+INFHR/24, NTIM))
  
  key.lst = c("TIMEPT", "NTIM")
  OUT = OUT[, c(key.lst, setdiff(colnames(OUT), key.lst))]
  
  # clean up 
  if(all(OUT$CYCLE==1)) {OUT = OUT %>% select(-CYCLE)}
  if(all(OUT$WEEK==1)) {OUT = OUT %>% select(-WEEK)}
  if(all(OUT$VISIT==1)) {OUT = OUT %>% select(-VISIT)}  
  
  if(all(OUT$DAY==0)) {OUT = OUT %>% select(-DAY)}
  if(all(OUT$HR==0)) {OUT = OUT %>% select(-HR)}
  if(all(OUT$MIN==0)) {OUT = OUT %>% select(-MIN)}
  
  if(all(is.na(OUT$PRE))) {OUT = OUT %>% select(-PRE)}
  if(all(is.na(OUT$POST))) {OUT = OUT %>% select(-POST)}
  
  
  return(OUT)
}


# to find anything outside () or within ()
#-----------------------------------------------------------
# s <- "A1{0}~B0{1} CO{a2}NN{12}" 
# gsub( "([^{}]*)\\{([^{}]*)\\}", "\\1 ", s ) 
# gsub( "([^{}]*)\\{([^{}]*)\\}", "\\2 ", s ) 
# gsub( "([^{}]*)\\(([^{}]*)\\)", "\\2 ", "WEEK 6 (DAY 43) " ) 







#unlist(regmatches(TIMEPT, gregexpr('\\(?[0-9,.]+', TIMEPT)))
# http://www.rexegg.com/regex-quickstart.html

#  "[[:digit:][:alpha:][:space:][:punct:]]+"

#   "The dog and the man play in the park.",
#   "The man plays with the dog.",
#   "That is the man's hat.",
#   "Man I love that dog!",
#   "I'm dog tired"
# )
# regexpr = "^(?=.*\\bman\\b)(?=.*\\bdog\\b)"
# grepl("^(?=.*\\bman\\b)(?=.*\\bdog\\b)", x, ignore.case=TRUE, perl=TRUE) 





parseTIMEPT2 <- function(TIMEPT, INFHR=1)  {  # old way
  
  # to parse TIMEPT like the following:
  #-------------------------------------
  TIMEPT = c(
    "D1 PRE",
    "D1 0HR",
    "D1 15MIN",
    "D1 30MIN",
    "D1 1HR",
    "D1 2HR",
    "D1 4HR",
    "D1 8HR",
    "D1 12HR",
    "D2", "D4", "D8", "D15", "D22", "D29")
  
  # assume having "TIMEPT" in data
  TIMEPT = toupper(data$TIMEPT) %>% unique()
  
  TIMEPT.ORG = TIMEPT
  TIMEPT = as.data.frame(TIMEPT)
  TIMEPT = TIMEPT %>% mutate(TIMEPT = gsub("HR", " HR", TIMEPT), 
                             TIMEPT = gsub("MIN", " MIN", TIMEPT) )
  
  TIMEPT = strVec2matrix(TIMEPT[[1]], sep=" ")
  colnames(TIMEPT) = c("DAY", "HOUR", "UNIT")
  TIMEPT = bind_cols(TIMEPT=TIMEPT.ORG, TIMEPT)
  
  # calculate NTIM = DAY+HOUR*CONST
  TIMEPT = TIMEPT %>% mutate(DAY=as.numeric(gsub("D","",DAY)), 
                             HOUR =ifelse(is.na(HOUR), 0, HOUR),
                             CONST=ifelse(is.na(UNIT), 1,  
                                          ifelse(UNIT=="MIN", 1/60/24, 
                                                 ifelse(UNIT=="HR", 1/24, NA))),
                             NTIM = DAY + as.numeric(HOUR)*CONST )
  
  # convert clinical Day 1 as NTIM=0, and so on.
  TIMEPT  = TIMEPT %>% mutate(NTIM=ifelse(NTIM>0, NTIM-1, NTIM))
  
  # if TIMEPT recorded from the end of infusion
  INFHR = 1 # assume 1 hour infusion if IV dose
  TIMEPT = TIMEPT %>% mutate(NTIM=ifelse(NTIM<1, NTIM+INFHR/24, NTIM))
  
  # assign NTIM=0 for PRE (pre-infusion)
  TIMEPT = TIMEPT %>% mutate(NTIM=ifelse(HOUR=="PRE", 0, NTIM))
  
  # final output
  data = data %>% left_join(TIMEPT[, c("TIMEPT", "NTIM")], by="TIMEPT")
  
  
}



