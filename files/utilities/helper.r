#' makes the data ranges
#' makes the data ranges with a small buffer
#'
#' @param dataColumn vector containing numeric elements
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  make_data_ranges(mtcars$disp)
make_data_ranges <- function(dataColumn) {
	maxdata <- max(dataColumn, na.rm=T)
	mindata <- min(dataColumn, na.rm=T)
	range <- maxdata - mindata
	
  # add a five percent space around all points
	c(mindata-0.05*range, maxdata+0.05*range)
}



#' make a plot object
#' make a plot object
#'
#' @param windowRanges Ranges of the plotting window
#' @return Returns a Qt plot object
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  make_new_plot(make_window_ranges(c(0,1,2,3)))
make_new_plot <- function(windowRanges,width=600,height=600){
	new_plot(width, height, xrange=windowRanges[1:2], yrange=windowRanges[3:4])
}	


#' make the window ranges
#' make the window ranges
#'
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @param xlab if xlab is replaced with somthing other than null, it will be assumed that an axis label will be used
#' @param ylab if ylab is replaced with somthing other than null, it will be assumed that an axis label will be used
#' @return returns a vector of four variables containing xmin, xmax, ymin, ymax
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  make_window_ranges(c(0,1,2,3))
make_window_ranges <- function(dataRanges, xlab=NULL, ylab=NULL, xtickmarks=NULL, ytickmarks=NULL, main=NULL) {

  # add more space for the Y label 
  if (!is.null(ylab)) {
    xmin = dataRanges[1] - 0.1*diff(dataRanges[1:2])
	} else {
    xmin = dataRanges[1] - 0.065*diff(dataRanges[1:2])
	}	
	xmax = dataRanges[2]+0.05*diff(dataRanges[1:2])
	
	
  # add more space for the X label 
	if (!is.null(xlab)) {
    ymin = dataRanges[3]-0.1*diff(dataRanges[3:4])
	} else {
    ymin = dataRanges[3]-0.065*diff(dataRanges[3:4])
	}
	ymax = dataRanges[4]+0.05*diff(dataRanges[3:4])

  # little extra space necessary for xtickmarks	
  if (!is.null(xtickmarks)) {
#    ymin = dataRanges[3]-0.05*diff(dataRanges[3:4])
    ymin = ymin - 0.05*diff(dataRanges[3:4])
  }

  # based on length of y tickmarks extra space
	if (!is.null(ytickmarks)) {
    xwidth = max(str_length(as.character(ytickmarks)))
    # each character gives 0.75% extra space
#    xmin = dataRanges[1] - 0.0075*xwidth*diff(dataRanges[1:2])		
    xmin = xmin - 0.0075*xwidth*diff(dataRanges[1:2])
  }

  # extra space for window title
  if (!is.null(main)) {
  	if (length(main)>0)
  	   ymax = ymax+0.05*diff(dataRanges[3:4])
  }

	windowRanges <- c(
    xmin,
    xmax,
    ymin,
    ymax
  )
	
#	if(if_bprint())
#    cat("Window Range: x=c(", windowRanges[1],", ",windowRanges[2],")  y=(",windowRanges[3],", ",windowRanges[4],")\n")
	
	windowRanges  
}

