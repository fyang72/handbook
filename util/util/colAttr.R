

#-----------------------------------------------------------------------------
# how to get the column attributes from a tibble object
#-----------------------------------------------------------------------------  

colAttr <- function(x, attrC, ifIsNull) {
  # Returns column attribute named in attrC, if present, else isNullC.
  atr <- attr(x, attrC, exact = TRUE)
  atr <- if (is.null(atr)) {ifIsNull} else {atr}
  atr
}


atribLst <- function(df, attrC, isNullC){
  # Returns list of values of the col attribute attrC, if present, else isNullC
  lapply(df, colAttr, attrC=attrC, ifIsNull=isNullC)
}

# stub93 <- AtribLst(cps_00093.df, attrC="label", isNullC=NA)




# class, mode, typeof, storage.mode, attributes, str,  
# A comprehensive survey of the types of things in R. 'mode' and 'class' and 'typeof' are insufficient
# http://stackoverflow.com/questions/6258004/types-and-classes-of-variables
#  http://stackoverflow.com/questions/8855589/a-comprehensive-survey-of-the-types-of-things-in-r-mode-and-class-and-type
col_modes <- function(obj) { 
  vapply(as.data.frame(head(obj)), mode,  character(1) ) }

col_types <- function(obj) { 
  vapply(as.data.frame(head(obj)), typeof,  character(1) ) }

col_class <- function(obj) { 
  vapply(as.data.frame(head(obj)), class,  character(1) ) }

#
#foo<-data.frame(x=c(1:10)*0.1, 
#                    y=c("red", "red", "red", "blue", "blue", 
#                        "blue", "yellow", "yellow", "yellow", 
#                        "green"),
#                    z= as.POSIXct(Sys.Date()+c(1:10)))
# col_types(foo)
#  
# foo %>% convert_colType(types = c("character", "character", "character"))
## 
#