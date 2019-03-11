

try_eval <-function(text="") {
## capture messages and errors to a file.
# https://stackoverflow.com/questions/11666086/output-error-warning-log-txt-file-when-running-r-script-under-command-line
# https://cran.r-project.org/web/packages/tryCatchLog/vignettes/tryCatchLog-intro.html
 
  
# create a folder locally if not exit
# temporarily switch to the temp dir, in case you do not have write
# permission to the current working directory
#setwd(tempdir())
owd <- tempdir()
on.exit(setwd(owd))  

zz <- file("all.Rout", open="wt")
sink(zz, type="message")


# source the function
try(
  eval(parse(text=text)) #, silent = TRUE
)  

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
message <- readLines("all.Rout")

#return(list(output=output, error_message=error_message))

#rm(owd, text, zz)
return(environment())

}