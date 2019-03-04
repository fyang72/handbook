

try_eval <-function(text="") {
## capture messages and errors to a file.
# https://stackoverflow.com/questions/11666086/output-error-warning-log-txt-file-when-running-r-script-under-command-line
# https://cran.r-project.org/web/packages/tryCatchLog/vignettes/tryCatchLog-intro.html
setwd(tempdir())

zz <- file("all.Rout", open="wt")
sink(zz, type="message")

# source the function
try(
  eval(parse(text=text))
)  

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
error.message <- readLines("all.Rout")

return(error.message)
}