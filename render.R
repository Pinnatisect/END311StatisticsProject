#!/usr/bin/env Rscript

library("rmarkdown")

render("document.Rmd", output_file="Document.pdf")
