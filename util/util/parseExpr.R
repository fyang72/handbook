
# https://www.regular-expressions.info/numericranges.html
extractExpr <- function(TIMEPT, regexpr='(D|DAY)(-|\\s+|)[0-9]+') {
  tt = regmatches(TIMEPT, gregexpr(regexpr, TIMEPT))
  DAY = do.call(rbind, lapply(tt, function(i) ifelse(length(i)==0, NA, i)))
  return(DAY)
}

removeExpr <- function(TIMEPT, regexpr='(D|DAY)(-|\\s+|)[0-9]+') {
  tt = regmatches(TIMEPT, gregexpr(regexpr, TIMEPT))
  DAY = do.call(rbind, lapply(1:length(tt), function(i)   ifelse(length(tt[[i]])==0, 
                                                                 TIMEPT[i], 
                                                                 trim(gsub(tt[[i]],"", TIMEPT[i], fix=TRUE)))    ))
  return(DAY)
}