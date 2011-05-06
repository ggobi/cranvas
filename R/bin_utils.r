# Make percents pretty
pretty_percent <- function(smallVal) {
    paste(round(100 * smallVal), "%", sep = "")
}


# find a column name unique to the plot
data_hist_column <- function(d) {
    add_unique_column(d, "qhist")
}
data_bin_column <- function(d) {
    add_unique_column(d, "qbin")
}
add_unique_column <- function(d, name) {
    dnames <- names(d)
    locations <- str_detect(dnames, str_c(name, "[1-9]*"))
    if (!any(locations)) {
        return(str_c(name, "1"))
    }
    histNames <- dnames[locations]
    
    histNumbers <- str_replace_all(histNames, name, "")
    histNumbers <- as.numeric(histNumbers)
    
    str_c(name, max(histNumbers) + 1)
}

# calculate the break positions including start and end
calcBinPosition <- function(start, width, maxDataPos, maxDrawPos) {
    # cat('start: ', start, '\twidth: ', width, '\tmaxDataPos: ', maxDataPos,
    #   '\tmaxDrawPos: ', maxDrawPos, '\n')
    output <- seq(from = start, to = maxDrawPos, by = width)
    if (max(output) < maxDataPos) {
        output <- c(output, maxDrawPos)
    }
    # cat('calcBinPosition: ', str_c(output, collapse = ', '), '\n')
    output
}

## find the range of the data
#dataRange <- function(d) {
#    range(d)
#}

# find the max binwidth of the data
maxBinwidth <- function(d, na.rm=T) {
    diff(range(d, na.rm=na.rm))/4
}

# execute a base histogram
baseHist <- function(d, ...) {
    output <- suppressWarnings(hist(d, plot = FALSE, ...))
    # print(output)
    output
}

# find the shifting unit when moving the start position
unitShift <- function(d, na.rm=T) {
    diff(range(d, na.rm=na.rm))/60
}

# find the largest deviation from the start position
maxShift <- function(d, na.rm=T) {
    0.1 * diff(range(d, na.rm=na.rm))
}

# find the maximum starting position
xMaxStartPos <- function(d, na.rm=T) {
    range(d, na.rm=na.rm)[1]
}

# find the minimum starting position
xMinStartPos <- function(d, na.rm=T) {
    range(d, na.rm=na.rm)[1] - maxShift(d)
}

# find the maximum ending position
xMaxEndPos <- function(d, na.rm=T) {
    range(d, na.rm=na.rm)[2] + maxBinwidth(d)
}

maxHeight <- function(d, na.rm=T, ...) {
    yMax <- 0
    tmpStartPos <- xMinStartPos(d, na.rm)
    maxStartPos <- xMaxStartPos(d, na.rm)
    # print(xColRange())
    while (tmpStartPos <= maxStartPos) {
        tB <- calcBinPosition(tmpStartPos, maxBinwidth(d, na.rm), range(d, na.rm=na.rm)[2], xMaxEndPos(d, na.rm))
        newMax <- max(baseHist(d, breaks = tB, ...)$counts)
        # print(newMax)
        if (newMax > yMax) 
            yMax <- newMax
        tmpStartPos <- tmpStartPos + unitShift(d)
    }
    yMax
}







 
