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
##' @importFrom qtbase qrect qfont qsize qconnect qdataFrameModel qtimer
##' @import qtpaint
##' @importFrom tourr basis_init basis_random cmass dependence_tour
##' freeze frozen_guided_tour frozen_tour geodesic_path grand_tour
##' guided_tour holes lda_pp little_tour local_tour new_geodesic_path
##' new_tour path_curves path_dist path_index paths_index pda_pp
##' planned_tour save_history planned_tour holes cmass
##' @export basis_init basis_random cmass dependence_tour freeze
##' frozen_guided_tour frozen_tour geodesic_path grand_tour
##' guided_tour holes lda_pp little_tour local_tour new_geodesic_path
##' new_tour path_curves path_dist path_index paths_index pda_pp
##' planned_tour save_history planned_tour holes cmass
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

##' Wages of male high-school dropouts
##'
##' The data was collected to track the labor experiences of male
##' high-school dropouts. The men were between 14 and 17 years old at
##' the time of the first survey.
##' @name wages
##' @docType data
##' @usage data(wages)
##' @format Number of subjects: 888; Number of variables: 15; Number
##' of observations, across all subjects: 6402
##'
##' \describe{ \item{\code{id}}{id numbers for each subject}
##' \item{\code{lnw}}{natural log of wages, adjusted for inflation, to
##' 1990 dollars} \item{\code{exper}}{length of time in the workforce
##' (in years). This is treated as the time variable, with \eqn{t = 0}
##' for each subject starting on their first day at work. The number
##' of time points and values of time points for each subject can
##' differ} \item{\code{ged}}{when/if a graduate equivalency diploma
##' is obtained} \item{\code{black}}{categorical indicator of race is
##' black} \item{\code{hispanic}}{categorical indicator of race is
##' hispanic} \item{\code{hgc}}{highest grade completed}
##' \item{\code{uerate}}{unemployment rates in the local geographic
##' region at each measurement time} }
##' @source Singer, J. D. & Willett, J. B. (2003), \emph{Applied
##' Longitudinal Data Analysis}, Oxford University Press, Oxford,
##' UK. It is a subset of data collected in the National Longitudinal
##' Survey of Youth (NLSY) described at
##' \url{http://www.bls.gov/nls/nlsdata.htm}.
##' @example inst/examples/wages-ex.R
NULL

##' Dataset of 2006 Australian Open mens tennis matches
##'
##' The data contains statistics from the 2006 Australian Open mens
##' tennis matches.
##' @name tennis
##' @docType data
##' @usage data(tennis)
##' @format data.frame: 25 obs. of  18 variables
##' @keywords datasets
##' @source 2006 Australian Open mens tennis matches.
##' @examples library(cranvas)
##' data(tennis); qtennis = qdata(tennis)
##'
##' qscatter(first.serve.pts, second.serve.pts, data = qtennis)
##' qscatter(matches, sets, data = qtennis)
NULL

##' Subset of data from the Behavioral Risk Factor Surveillance System
##'
##' Part of the largest, on-going telephone health survey system,
##' tracking health conditions and risk behaviors in the United States
##' yearly since 1984. This data has a lot of missing values, so it is
##' used for testing the missing value plots.
##' @name brfss
##' @docType data
##' @usage data(brfss)
##' @format data.frame: 245 obs. of  409 variables
##' @keywords datasets
##' @source http://www.cdc.gov/BRFSS/
##' @examples library(cranvas)
##' data(brfss); qbrfss = qdata(brfss)
##'
##' qmval(names(brfss)[40:50])
##' qmval(51:68)
##' qmval(~POORHLTH+FRUIT+GREENSAL)
NULL

##' US Crimes data from 2009
##'
##' Counts of different crimes by state across the USA
##' @name crimes
##' @docType data
##' @usage data(crimes)
##' @format data.frame: 50 obs. of 13 variables
##' @keywords datasets
##' @source http://www.fbi.gov/about-us/cjis/ucr/ucr
##' @examples library(cranvas)
##' data(crimes)
##' crimes$State <- factor(tolower(crimes$State))
##' qcrimes <- qdata(crimes)
##' qparallel(data=qcrimes)
NULL

##' Spatiotemporal measurements of climate variables
##'
##' Monthly measurements from 1995-2000 of temperature, pressure
##' ozone and clouds over central America. The data was provided
##' for the 2006 ASA Stat Computing and Graphics Data Expo competition.
##' @name nasa
##' @docType data
##' @usage data(nasa)
##' @format data.frame: 50 obs. of 13 variables
##' @keywords datasets
##' @source http://stat-computing.org/dataexpo/2006/
##' @examples library(cranvas)
##' data(nasa)
##' nasa11 <- subset(nasa, Gridx == 22 & Gridy == 21)
##' qnasa <- qdata(nasa11)
##' qtime(TimeIndx,~ts,qnasa,shift=c(1,12))
NULL

##' Temporal measurements on UK Pig production
##'
##' Multivariate time series data originally from Andrews and Herzberg (1985).
##' @name pigs
##' @docType data
##' @usage data(pigs)
##' @format data.frame: 48 obs. of 11 variables
##' @keywords datasets
##' @source http://lib.stat.cmu.edu/datasets/Andrews/
##' @examples library(cranvas)
##' data(pigs)
##' qpig <- qdata(pigs)
##' qtime(TIME, ~GILTS+PROFIT+PRODUCTION+HERDSZ, qpig, shift=c(1,4))
NULL

##' Coordinates of the world map
##'
##' An shortened version of the map coordinates for all the countries
##' on the globe. Polygon edges will be a bit rough, but the speed is
##' improved for interaction.
##' @name nasa
##' @docType data
##' @usage data(world)
##' @format data.frame: 48 obs. of 11 variables
##' @keywords datasets
##' @source maps package
##' @examples library(cranvas)
##' data(world)
##' qworld <- qdata(world)
##' qmap(qworld, long, lat, group, label = id)
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
