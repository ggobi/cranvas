# Plan for the code:
# Use the tour engine to produce the projection coefficients
# Add columns to the data, .proj1, .proj2 to contain the projected data
# Plot using the scatterplot function
# Do I need to create a new qanimate, qdisplay function for the tour????
# How do I get beyond dataframes to mutaframes?
qtour_xy <- function(qdata, tour_path = grand_tour(), ...) {

  bases <- qanimate(qdata, tour_path, display_xy(...))
  cat("tour",dim(bases),"\n")
}

qanimate <- function(qdata, tour_path = grand_tour(), display = display_xy(), start = NULL, aps = 1, fps = 30, max_frames = Inf, rescale = TRUE, sphere = FALSE, ...) {
  # Will need to do this at some point, but this doesn't work on mutaframes yet
  # if (rescale) data <- rescale(data)
  # if (sphere) data  <- sphere(data)
  
  # By default, only take single step if not interactive
  # This is useful for the automated tests run by R CMD check
  if (!interactive() && missing(max_frames)) {
    max_frames <- 1
  }
  if (max_frames == Inf) {
    to_stop()
  }

  data <- data.frame(qdata)
  tour <- new_tour(data, tour_path, start)
  start <- tour(0)
  bs <- 1
  bases <- array(NA, c(ncol(data), ncol(start$target), bs))

  # Initialise display
  #display$init(data)
  #display$render_frame()
  #display$render_data(data, start$proj, start$target)
  os <- find_platform()$os
  
  b <- 0
  i <- 0
  
  tryCatch({
    while(i < max_frames) {
      i <- i + 1
      step <- tour(aps / fps)
      if (is.null(step)) break
      if (step$step == 1) {
        b <- b + 1
        if (b > bs) {
          bases <- c(bases, rep(NA, bs * dim(bases)[1] * dim(bases)[2]))
          dim(bases) <- c(ncol(data), ncol(start$target), 2 * bs)
          bs <- 2 * bs
        }
        bases[, , b] <- step$target
      }
    
      #if (os == "win") {
      #  display$render_frame()
      #} else {
      #  display$render_transition()
      #}
      #display$render_data(data, step$proj, step$target)
    
      Sys.sleep(1 / fps)    
    }
  }, interrupt = function(cond) return())
  
  invisible(bases[, , seq_len(b)])
}

# This function needs to be made visible from the tourr package in order to use
to_stop <- function() {
  plat <- find_platform()
  if(plat$os == "win") {
    key <- "Esc"
  } else if (plat$os == "mac" && plat$iface == "gui") {
    key <- "Esc"
  } else {
    key <- "Ctrl + C"
  }
  message("Press ", key, " to stop tour running")
}
