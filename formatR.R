#!/usr/bin/env Rscript

p = grep("--file=", commandArgs(), fixed = TRUE, value = TRUE)
if (length(p) == 1) {
    p = dirname(sub("^--file=", "", p))
} else p = getwd()

if (!require("formatR")) install.packages("formatR", repos = "http://cran.r-project.org")

tidy.dir(p, recursive = TRUE, keep.blank.line = TRUE, width.cutoff = 80) 
