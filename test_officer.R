

source("~/YANG/global.R")

# extract number from string using "regular expression"

astr <- c("April ( 123widgets less 456 sprockets )", 
"May (789 widgets less 012 sprockets)", 
"30 mg/kg IV Q3w")

matches <- regmatches(astr, gregexpr("[[:digit:]]+", astr))
as.numeric(unlist(matches))

arma_lst <- c("30 mg/kg IV Q3W", 
              "100 mg/kg SC Q6W", 
              "1 mg/kg SC QW + 3 mg/kg IV Q2W"
)
parseARMA(arma_lst) %>% arrange(route)



  ^     # start of string
  \s*   # optional whitespace
  (\w+) # one or more alphanumeric characters, capture the match
  \s*   # optional whitespace
  \(    # a (
  \s*   # optional whitespace
  (\d+) # a number, capture the match
  \D+   # one or more non-digits
  (\d+) # a number, capture the match
  \D+   # one or more non-digits
  \)    # a )
  \s*   # optional whitespace
  $     # end of string
  
  
  
  
# https://www.pharmasug.org/proceedings/tokyo2018/presentations/PharmaSUG-Tokyo-2018-05.pdf

library(dplyr)
 library(ggplot2)
 library(magrittr)
 library(knitr)
 library(xtable)
 library(flextable)
 library(officer)

myplot <- ggplot(data=iris, aes(Sepal.Length, Sepal.Width)) +
   geom_point()
 myppt <- read_pptx()
 
 mylab <- layout_summary(myppt)[[1]] # Slide Layout Name
 mytmp <- layout_summary(myppt)[[2]][1] # Show Slide Master Name
 
 myppt <- myppt %>%
   add_slide(layout="Title Slide", master=mytmp) %>%
   ph_with_text(type="ctrTitle", str="Iris Data") %>%
   ph_with_text(type="subTitle", str="Table & Figure") %>%
   ph_with_text(type="dt", str=format(Sys.Date())) %>%
   add_slide(layout="Title and Content", master=mytmp) %>%
   ph_with_text(type="title", str="Scatter Plot") %>%
   ph_with_gg(value=myplot) %>%
   add_slide(layout="Two Content", master=mytmp) %>%
   ph_with_text(type="title", str="Table") %>%
   ph_with_table(type="body", value=iris[1:5,], index=1) %>%
   ph_with_text(type="body", str="Iris data", index=2)
 
 print(myppt, target="./sample.pptx")
 browseURL("./sample.pptx")




myplot <- ggplot(data=iris, aes(Sepal.Length, Sepal.Width)) +
  + geom_point()

 mydoc <- read_docx() %>%
   body_add_par(value="Table of contents", style="heading 1") %>%
   body_add_toc(level=2) %>%
   body_add_break() %>%
   body_add_par(value="Iris Data", style="heading 1") %>%
   body_add_par(value="Table", style="heading 2") %>%
   body_add_par("Sepal.Length vs. Sepal.Width", style="Normal") %>%
   body_add_table(value=iris[1:5,], style="table_template" ) %>%
   body_add_par(value="Scatter Plot", style="heading 2") %>%
   body_add_gg(value=myplot, style="centered")
print(mydoc, target="./sample.docx")
browseURL("./sample.docx")




