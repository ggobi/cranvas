 extract.formula <- function(form) {
    if( length(form) == 2 ) {
      formstring <- paste(form[[1]], form[[2]])
    }
    if( length(form) == 3 ) {
      formstring <- paste( form[[2]], form[[1]], form[[3]])
    }    
    return(formstring)
    
  }

  find_xid <- function( data, colName) {
    cols <- subset(data, select = colName)[,1]
    if(!(length(levels(cols[1])) == 0)) {
      xid <- levels(cols[1])
    } else if (class (cols[1]) == "numeric" || class(cols[1]) == "integer") {
      xid <- pretty(cols)
    }
    return(xid)
  }
  
  find_yid <- function( data, colName) {
    cols <- subset(data, select = colName)[,1]
    if(!(length(levels(cols[1])) == 0)) {
      yid <- levels(cols[1])
    } else if (class (cols[1]) == "numeric" || class(cols[1]) == "integer") {
      yid <- pretty(cols)
    } else {
      stop("data type not supported")
    }
    
    return(yid)
  }
  
  get_axisPosX <- function(data, colName) {
    cols <- subset(data, select = colName)[,1]
    if(!(length(levels(cols[1])) == 0)) {
      by <- 1/(length(levels(cols[1])) + 1)
      majorPos <- seq.int(c(0:1), by = by)
    } else if (class (cols[1]) == "numeric" || class(cols[1]) == "integer") {
      majorPos <- pretty(cols)
    } else {
      stop("data type not supported")
    }
    
    return(majorPos)
  } 

  get_axisPosY <- function(data, colName) {
    cols <- subset(data, select = colName)[,1]
    if(!(length(levels(cols[1])) == 0)) {
      by <-1/(length(levels(cols[1])) + 1)
      majorPos <- seq.int(c(0:1), by = by)
    } else if (class (cols[1]) == "numeric" || class(cols[1]) == "integer") {
      majorPos <- pretty(cols)
    } else {
      stop("data type not supported")
    }
    
    return(majorPos)
  } 

