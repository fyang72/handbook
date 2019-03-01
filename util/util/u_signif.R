u_signif <- function(x, digits=3) {
  #noquote(sapply(a,function(x) format(signif(x, n), scientific=FALSE)))
  
  #out1 <- matrix(prettyNum(signif(x, digits=n), scientific=FALSE), nrow=nrow(x), ncol=ncol(x))
  
  x <- data.matrix(x)
  x <- matrix(suppressWarnings(as.numeric(as.character(x))), nrow=nrow(x), ncol=ncol(x),dimnames =list(rownames(x), colnames(x)))         #list(row_names, col_names)
  
  out1 <- formatC(signif(x,digits=digits), digits=digits,format="fg", flag="#")     # + - #
  ids <- which(substr(out1, nchar(out1), nchar(out1))==".")
  out1[ids] <- substr(out1[ids], 1, nchar(out1[ids])-1)
  
  #x <- out1
  #out1 <- matrix(as.numeric(x), nrow=nrow(x), ncol=ncol(x),dimnames =list(rownames(x), colnames(x)))         #list(row_names, col_names)
  
  noquote(out1)
  
  
  return(out1)
  
  # return(noquote(format(signif(x,n), scientific=FALSE)))
  
  #a <- c(10.00001,12345,1234.5,123.45,1.2345,0.12345)
  #  print(formatC(signif(a,digits=3), digits=3,format="fg", flag="#"))
  
}





u.signif <- function(x, digits=3) {
  #noquote(sapply(a,function(x) format(signif(x, n), scientific=FALSE)))
  
  #out1 <- matrix(prettyNum(signif(x, digits=n), scientific=FALSE), nrow=nrow(x), ncol=ncol(x))
  
  x <- data.matrix(x)
  x <- matrix(suppressWarnings(as.numeric(as.character(x))), nrow=nrow(x), ncol=ncol(x),dimnames =list(rownames(x), colnames(x)))         #list(row_names, col_names)
  
  out1 <- formatC(signif(x,digits=digits), digits=digits,format="fg", flag="#")     # + - #
  ids <- which(substr(out1, nchar(out1), nchar(out1))==".")
  out1[ids] <- substr(out1[ids], 1, nchar(out1[ids])-1)
  
  #x <- out1
  #out1 <- matrix(as.numeric(x), nrow=nrow(x), ncol=ncol(x),dimnames =list(rownames(x), colnames(x)))         #list(row_names, col_names)
  
  noquote(out1)
  
  
  return(out1)
  
  # return(noquote(format(signif(x,n), scientific=FALSE)))
  
  #a <- c(10.00001,12345,1234.5,123.45,1.2345,0.12345)
  #  print(formatC(signif(a,digits=3), digits=3,format="fg", flag="#"))
  
}

