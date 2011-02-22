#!/usr/bin/env Rscript

## this script will automatically run roxygen on cranvas; it can be used as a shell script
## e.g. Rscript roxygenize.R or Rscript roxyenize.R update

## extract the working directory from the file you provided
##  e.g. Rscript ~/pkg/cranvas/roxygenize.R
p = grep('--file=', commandArgs(), fixed = TRUE, value = TRUE)
if (length(p) == 1) {
    p = dirname(sub('^--file=', '', p))
    setwd(p)
}

## make sure the working directory is under cranvas
if (!('cranvas' %in% list.files('../')))
    stop('the cranvas package not found under ', normalizePath(file.path(getwd(), '..')))

## update git as well; someone wants to be really lazy
if ('update' %in% commandArgs(TRUE)) system('git pull')

## requires Rd2roxygen and formatR (don't ask why; just do it)
if (!require('Rd2roxygen')) install.packages('Rd2roxygen')
if (!require('formatR')) install.packages('formatR')

## remove the man directory first
unlink('man', recursive = TRUE)

## go up a level
owd = setwd('..')

library(Rd2roxygen)
options(width = 75)

## run roxygen and several cleaning up steps
try(rab('cranvas', install = TRUE))

setwd(owd)
