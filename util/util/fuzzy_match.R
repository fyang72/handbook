fuzzy_match <- function(VAR, contactTab=contactInput()) {
  # 
  # VAR = c("YUN Zheng", "FENG YANG", "楊峰", "葉培源 /Pei-Yuan Yeh", 
  #         "嚴緻廽/Yen,Sharon",
  #         "何　菁/ Jine He ",
  #         "汪淑芳/ Shuefung Wang",
  #         "王昇华/ Barbara Wang ",
  #         "葉培源/ Pei-Yuan Yeh",
  #         "范承瑋/ Lydia Fan",
  #         "汪淑芳/ Shuefung Wang")
  
  # y=c("Bruce Almighty", "Lee, Bruce", "Leroy Brown")
  # y2 <- sub("(.*) (.*)", "\\2 \\1", y, perl=TRUE)
  # 
  # agrep("Bruce Lee", y)  # No match
  # agrep("Bruce LEE", y2) # Match!
  #  
  
  # It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
  if (length(VAR)==0) {return(NULL)}
  if (is.null(VAR[1])) {return(NULL)}
  if (is.na(VAR[1])) {return(NULL)}
  
  dist.name<-adist(VAR, contactTab$ALL_NAME, partial = TRUE, ignore.case = TRUE)
  
  # We now take the pairs with the minimum distance
  min.name<-apply(dist.name, 1, min)
  
  match.s1.s2<-NULL  
  for(i in 1:nrow(dist.name))
  {
    s2.i<-match(min.name[i],dist.name[i,])
    s1.i<-i
    match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,
                                  contactTab[s2.i, c("CN_NAME", "EN_NAME", "EMAIL" )], 
                                  VAR=VAR[s1.i], adist=min.name[i]),match.s1.s2)
  }
  # and we then can have a look at the results
  return(match.s1.s2)
}