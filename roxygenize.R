#!/usr/bin/env Rscript

## this script will automatically run roxygen on cranvas; it can be used as a shell script
## e.g. 'Rscript roxygenize.R' or 'Rscript roxyenize.R update' to run 'git pull' before roxygenizing
## run roxygen and check the package: Rscript roxygenize.R check

## extract the working directory from the file you provided
##  e.g. Rscript ~/pkg/cranvas/roxygenize.R
p = grep("--file=", commandArgs(), fixed = TRUE, value = TRUE)
if (length(p) == 1) {
    p = dirname(sub("^--file=", "", p))
    setwd(p)
}

## make sure the working directory is under cranvas
if (!("cranvas" %in% list.files("../"))) stop("the cranvas package not found under ",
    normalizePath(file.path(getwd(), "..")))

## update git as well; someone wants to be really lazy
if ("update" %in% commandArgs(TRUE)) system("git pull --rebase")

try(update.packages(.libPaths()[1], ask = FALSE, repos = 'http://cran.r-project.org'))

## use Rd2roxygen to roxygenize cranvas

## go up a level
owd = setwd("..")

library(Rd2roxygen)
library(digest)
options(width = 80, replace.assign = TRUE)

## run roxygen and several cleaning up steps
unlink('cranvas/man', recursive = TRUE)
try(rab("cranvas", build = TRUE, install = TRUE, check = "check" %in% commandArgs(TRUE)))

setwd(owd)
