#' This is not a display method, but a method for adding 
tourr <- function() {
  
  
}

# TODO:
#   Respond to changes in underlying data, fps, vars, etc
#  

#' @examples
#' flea <- rescaler(flea)
#' qflea <- qdata(flea)
#' flea_tour <- Tourr$new(qflea, grand_tour(3), 1:6)
#' flea_tour$step()
#' qparallel(c("tour_1", "tour_2", "tour_3"), qflea)
#' qscatter(qflea, tour_1, tour_2)
#' # qhist(qflea, "tour_1")
#' flea_tour$start()
#' flea_tour$pause()

Tourr <- setRefClass("Tourr", 
  c("dest", "src", "tour_path", "tour", "aps", "paused", "timer", "last_time")
)
    
Tourr$methods(initialize = function(data, tour_path, vars, aps = 1, fps = 30) {
  stopifnot(is.mutaframe(data))

  src <<- as.matrix(as.data.frame(data[, vars]))
  dest <<- data

  tour_path <<- tour_path
  paused <<- TRUE
  aps <<- aps
  last_time <<- NULL

  timer <<- qtbase::qtimer(1000 / fps, .self$step)
  tour <<- new_tour(src, tour_path, NULL)
  
  .self
})

Tourr$methods(step = function() {

  if (is.null(last_time)) {
    last_time <<- proc.time()[3]
    delta <- 0
  } else {
    cur_time <- proc.time()[3]
    delta <- (cur_time - last_time)    
    last_time <<- cur_time
  }

  tour_step <- tour(aps * delta)
  if (is.null(tour_step$proj)) {
    pause()
    return()
  }

  data_proj <- src %*% tour_step$proj
  data_proj <- scale(data_proj, center = TRUE, scale = FALSE)
  colnames(data_proj) <- paste("tour", 1:ncol(data_proj), sep = "_")

  for(col in colnames(data_proj)) {
    dest[[col]] <<- data_proj[, col]
  }

  invisible(tour_step)
})


Tourr$methods(pause = function() {
  timer$stop()  
})
Tourr$methods(start = function() {
  timer$start()  
})
