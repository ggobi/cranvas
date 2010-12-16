new_slider <- function(min, max, inc, start = mean(min, max)) {
  value <- start
  
  inc_f <- function() {
    value <<- min(max, value + inc)
    self
  }
  dec_f <- function() {
    value <<- max(min, value - inc)
    self
  }
  val_f <- function() {
    value
  }
  
  self <- list(inc = inc_f, dec = dec_f, val = val_f)
}