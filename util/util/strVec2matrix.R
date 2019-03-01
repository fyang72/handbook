
strVec2matrix <- function(x, sep=" ", fill="right") {
  library(tidyr)
  dat = data.frame(x)
  
  ncol <- max(sapply(strsplit(as.character(dat$x), sep), length))
  out= dat %>% separate(x, paste0("V", seq(1,ncol)), sep=sep, fill=fill)
  return(out)
}

