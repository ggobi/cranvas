#!/usr/bin/env Rscript

## this script will automatically run roxygen on cranvas; it can be used as a shell script
## e.g. 'Rscript roxygenize.R' or 'Rscript roxyenize.R update' to run 'git pull' before roxygenizing
## other options beside update include: build, install and check

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

if (NROW(old.packages(repos = 'http://cran.r-project.org')) && (!interactive() ||
    select.list(c('Yes', 'No'), title = 'Update old R packages?', preselect = 'Yes') == 'Yes')) {
    try(update.packages(ask = FALSE, repos = 'http://cran.r-project.org'))
}
## use Rd2roxygen to roxygenize cranvas
if (!require("devtools")) install.packages("devtools", repos = "http://cran.r-project.org")
if (!require("formatR")) install.packages("formatR", repos = "http://cran.r-project.org")

if (!grepl('roxygen2', packageDescription('Rd2roxygen', fields = 'Depends'))) {
    library(devtools)
    install_github('roxygen2', 'klutometis')
    install_github('Rd2roxygen', 'yihui')
}

## go up a level
owd = setwd("..")

library(Rd2roxygen)
options(width = 80, replace.assign = TRUE)

## run roxygen and several cleaning up steps
try(rab("cranvas", build = "build" %in% commandArgs(TRUE),
        install = "install" %in% commandArgs(TRUE),
        check = "check" %in% commandArgs(TRUE)))

setwd(owd)
