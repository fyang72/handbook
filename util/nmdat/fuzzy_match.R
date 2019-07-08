  
 # adsl = read_csv("./data/adsl.csv")  
 # print(fuzzy_match(toupper(adsl$RACE)%>% unique(), race_var_lst))
 # 
 # s1 = c("Whiteee", "", NA)
 # s2 = c("White", "Black")
 # fuzzy_match(s1, s2, method = "jw")
 # 
fuzzy_match <- function(str_vec="", std_name="", 
                        method = "jw", threshold = 0.50) {
    
  validate(need(std_name, message="std_name in fuzzy_match is empty."),
           need(str_vec, message="str_vec in fuzzy_match is empty.")
           )
  
 tt = fuzzy_match0(str_vec, std_name, method = method)
 tt = cbind(str_vec, tt[match(str_vec, tt$s1name), ])
 tt %>% rename(score = one_of(method)) %>% 
   mutate(score = ifelse(is.na(score) | score>threshold, NA, score)) %>% 
   mutate(s2name = ifelse(is.na(score), NA, s2name), 
          s2name = ordered(s2name, levels=std_name)) %>% 
   pull(s2name)
}
 

  
#https://www.r-bloggers.com/fuzzy-string-matching-a-survival-skill-to-tackle-unstructured-information/

fuzzy_match0 <- function(s1="", s2="", method = "jw") {
  # c('osa','lv','dl', 'lcs','qgram','cosine','jaccard','jw')
  # Method 2: applying different string matching methods
  #osa Optimal string aligment, (restricted Damerau-Levenshtein distance).
  #lv Levenshtein distance (as in R’s native adist).
  #dl Full Damerau-Levenshtein distance.
  #hamming Hamming distance (a and b must have same nr of characters).
  #lcs Longest common substring distance.
  #qgram q-gram distance.
  #cosine cosine distance between q-gram profiles
  #jaccard Jaccard distance between q-gram profiles
  #jw Jaro, or Jaro-Winker distance.
  
  #install.packages('stringdist')
  library(stringdist)
  
  distance.methods<-method # c('osa','lv','dl', 'lcs','qgram','cosine','jaccard','jw')
  dist.methods<-list()
  for(m in 1:length(distance.methods))
  {
    dist.name.enh<-matrix(NA, ncol = length(s2),nrow = length(s1))
    for(i in 1:length(s2)) {
      for(j in 1:length(s1)) { 
        dist.name.enh[j,i]<-stringdist(tolower(s2[i]),tolower(s1[j]),method = distance.methods[m])      
        #adist.enhance(s2[i,],s1[j,])
      }  
    }
    dist.methods[[distance.methods[m]]]<-dist.name.enh
  }
  
  match.s1.s2.enh<-NULL
  for(m in 1:length(dist.methods))
  {
    dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
    min.name.enh<-apply(dist.matrix, 1, base::min)
    for(i in 1:nrow(dist.matrix))
    {
      s2.i<-match(min.name.enh[i],dist.matrix[i,])
      s1.i<-i
      match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,
                                        s2name=s2[s2.i], 
                                        s1name=s1[s1.i], 
                                        adist=min.name.enh[i],
                                        method=distance.methods[m]
      ),
      match.s1.s2.enh)
    }
  }
  # Let's have a look at the results
  library(reshape2)
  matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
  
  matched.names.matrix = matched.names.matrix %>% 
    mutate(s1name=as.character(s1name),
           s2name=as.character(s2name)
                                )
  return(matched.names.matrix)
}


# 
# s1 = c("FEMAL", "sFe3434uyle", "bsdfsgf", "MALE", "FEMALE1", "F", "M", "f", "m")
# s2 = c("FEMALE", "MALE")
# fuzzy_match(s1, s2, method = "jw")
 