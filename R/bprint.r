#' Global Printing Check
#' checks to see if the print statements should be executed
#'
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  if(if_bprint()) cat("Hello World")
if_bprint <- function() {
  # TRUE
  FALSE
}

#' Print Item Information
#' prints the item name and structure information below it
#'
#' @param ... item to be quoted and then printed
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  dataRanges <- c(0,1,2,3)
#'  bprint(dataRanges)
bprint <- function(...) {
  if(if_bprint()) {
    cat("\n",substitute(...),":\n")
    str(...)
  }
}
