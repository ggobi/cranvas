##' Interactive statistical graphics based on Qt
##'
##' This package was designed mainly for interactions in statistical
##' plots, a feature (nearly) missing in \R for long. It contains most
##' common statistical plots like histograms, scatter plots, bar
##' charts, parallel coordinates plots, density plots, mosaic plots,
##' boxplots, maps, missing value plots and time series plots. All
##' plots support some common interactions as well as plot-specific
##' interactions with the keyboard or the mouse.
##'
##' The actual drawing is based on two packages \pkg{qtbase} and
##' \pkg{qtpaint}, which connect \R to Qt. The data structure is based
##' on \pkg{plumbr} and \pkg{objectSignals}; the
##' \code{\link[plumbr]{mutaframe}}s and reference classes are used
##' extensively in this package. Usually there are listeners
##' (\code{\link[plumbr]{add_listener}}) and signaling fields
##' (\code{\link[objectSignals]{signalingFields}}) attached to data
##' objects (created by \code{\link{qdata}}), so the plots can listen
##' to the changes in data (hence get updated). Note all the plots
##' based on the same data object are linked by default, so the
##' interactions in one plot will be reflected in other plots as well.
##'
##' A plot can be in either the brush mode (default) or the identify
##' mode. In the brush mode, we can use a rectangle brush to select
##' elements in the plot; in the identify mode, the information about
##' the identified elements under the mouse will be shown in the plot.
##'
##' See \code{\link{common_mouse_press}},
##' \code{\link{common_mouse_move}},
##' \code{\link{common_mouse_release}}, \code{\link{common_key_press}}
##' and \code{\link{common_key_release}} for common interactions in
##' all plots, and the documentation of specific plots for other
##' possible interactions.
##' @importFrom qtbase Qt
##' @importFrom qtbase qrect qfont qsize qconnect qdataFrameModel
##' @import qtpaint
##' @import plumbr
##' @import objectSignals
##' @name cranvas-package
##' @aliases cranvas
##' @docType package
##' @example inst/examples/cranvas-ex.R
NULL

##' NRC rankings data for the statistics departments in the US
##'
##' This data contains the NRC rankings for all the statistics
##' departments in US.
##'
##' @name nrcstat
##' @docType data
##' @usage data(nrcstat)
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
