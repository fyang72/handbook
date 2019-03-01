

###############################################################
# How to convert all INT to numeric apply to all columns
###############################################################

#Suppose you want to transform all your int columns to numeric, you can do so using one pipe:
#myDf %>%  mutate_each_( funs(as.numeric(.)), names( .[,sapply(., is.integer)] ))

# head(  pkpd %>% mutate_each(funs(replace(., .=="." , "BLOQ"))  ))
replace_NA <- function(df, value=0.078, by="BLQ") {
  df  %>% mutate_each(funs(replace(., .==0.078, by))  )  }
