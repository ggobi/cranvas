docs:
	rm -r man/;\
	R -q -e 'options(replace.assign=TRUE, width=80); library(Rd2roxygen); setwd(".."); rab("cranvas", install = TRUE)'

