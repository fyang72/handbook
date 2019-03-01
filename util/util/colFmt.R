

#https://statnmap.com/2017-04-04-format-text-conditionally-with-rmarkdown-chunks-complete-iframed-article/
colFmt = function(x, color){
  outputFormat =  opts_knit$get("rmarkdown.pandoc.to")
  outputFormat = ifelse(is.null(outputFormat), "html", outputFormat) # default html
  
  if(outputFormat == 'latex')
    (paste("\\textcolor{",color,"}{",x,"}",sep=""))
  else if(outputFormat == 'html')
    (paste("<font color='",color,"'>",x,"</font>",sep=""))
  else
    (x)
}
