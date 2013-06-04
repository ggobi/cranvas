docs:
	rm -r man/;\
	R -q -e 'options(replace.assign=TRUE, width=80); library(Rd2roxygen); setwd(".."); rab("cranvas", install = TRUE)'

qt:
	cd ../qtbase; git pull; cd ..; rm -f qtbase_*.tar.gz; R CMD build qtbase; R CMD INSTALL qtbase_*.tar.gz;\
	cd qtpaint; git pull; cd ..; rm -f qtpaint_*.tar.gz; R CMD build qtpaint; R CMD INSTALL qtpaint_*.tar.gz
