docs:
	Rscript -e 'options(replace.assign=TRUE)' -e 'library(Rd2roxygen)'\
	        -e 'setwd("..")' -e 'rab("cranvas", install = TRUE)'

qt:
	cd ../qtbase; git pull; cd ..; rm -f qtbase_*.tar.gz; R CMD build --no-build-vignettes qtbase; R CMD INSTALL qtbase_*.tar.gz;\
	cd qtpaint; git pull; cd ..; rm -f qtpaint_*.tar.gz; R CMD build --no-build-vignettes qtpaint; R CMD INSTALL qtpaint_*.tar.gz
