##' Interactive Statistical Graphics Based on Qt
##'
##' @importFrom qtbase Qt
##' @importFrom qtbase qrect qfont qsize qconnect qdataFrameModel
##' @import qtpaint
##' @import plumbr
##' @import objectSignals
##' @name cranvas-package
##' @docType package
NULL

##' NRC Rankings Data for the Statistics Departments in US
##'
##' This data contains the NRC rankings for all the statistics departments in US.
##'
##' @name nrcstat
##' @docType data
##' @format data.frame: 61 obs. of  68 variables
##' @keywords datasets
##' @source \url{http://sites.nationalacademies.org/pga/resdoc/index.htm}
##' @examples
##' data(nrcstat)
##' summary(nrcstat)
NULL


## set options(cranvas_debug = TRUE) to print the debug message

cranvas_debug = function(msg) {
    if (isTRUE(getOption("cranvas_debug"))) {
        if (missing(msg))
            msg = paste("calling", as.character(sys.call(1)[1]))
        message(msg)
        cat("##------ ", date(), " ------##", "\n\n")
        flush.console()
    }
}
