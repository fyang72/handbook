#!/bin/sh

bookdown::render_book(c('index.Rmd'), 'bookdown::word_document2')
#Rscript -e "bookdown::render_book('index.Rmd', '01-intro.Rmd', 'bookdown::gitbook')"
