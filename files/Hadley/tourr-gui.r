# source("~/Documents/cranvas/demos/tourr-gui.r"); gui_xy(olive)

library(qtbase)
library(qtpaint)
library(ggplot2, warn.conflicts = FALSE)
library(tourr, warn.conflicts = FALSE)
olive$region <- factor(olive$region)
library(colorspace)
library(RGtk2)
library(gWidgets)

gui_xy <- function(data = flea, ...) {
  num <- sapply(data, is.numeric)
  
  tour <- NULL
  tour_anim <- NULL
  prev_var <- NULL
  update_tour <- function(...) {
    if (!identical(prev_var, svalue(Variables))) {
      cur_proj <<- NULL
      prev_var <<- svalue(Variables)
    }

    tour <<- create_tour(data, 
      var_selected = svalue(Variables), 
      cat_selected = svalue(Class), 
      axes_location = svalue(dl),
      tour_type = svalue(TourType),
      aps = svalue(sl),
      cur_proj = cur_proj
    )
    
    
    tour_anim <<- with(tour, new_tour(data, tour_path, cur_proj))
    TRUE
  }

  data_proj <- NULL
  cur_proj <- NULL
  delta <- NULL
  delta_sm <- NULL
  last_time <- proc.time()[3]
  step_tour <- function(...) {
    # if there's no tour, don't draw anything
    if (is.null(tour)) {
      pause(TRUE)
      return()
    }
    
    cur_time <- proc.time()[3]
    delta <<- (cur_time - last_time)
    if (is.null(delta_sm)) delta_sm <<- delta
    # Should pick smoothing constant (alpha) so that an average is basically
    # over the previous one or two seconds
    delta_sm <<- delta_sm * 0.95 + delta * 0.05
    last_time <<- cur_time

    tour_step <- tour_anim(svalue(sl) * delta)
    if (is.null(tour_step$proj)) {
      pause(TRUE)
      return()
     }

    cur_proj <<- tour_step$proj
    
    data_proj <<- tour$data %*% tour_step$proj
    data_proj <<- scale(data_proj, center = TRUE, scale = FALSE)
    qupdate(points)
  }

  render_tour <- function(item, painter, exposed) {
    col <- alpha(tour$colour, svalue(sl_alpha))
    size <- svalue(sl_size)
    if (size < 0.5) {
      qdrawPoint(painter, data_proj[, 1], data_proj[,2], stroke = col)      
    } else {
      circle <- qglyphCircle(size)
      qstrokeColor(painter) <- NA
      qdrawGlyph(painter, circle, data_proj[, 1], data_proj[,2], fill = col)
    }
    
    if (!is.null(delta)) {
      qstrokeColor(painter) <- "black"
      qdrawText(painter, sprintf("%.1f", 1 / delta_sm), 1, 1, "right", "top")      
    }
  }
  
  render_axes  <- function(item, painter, exposed) {
    if (is.null(cur_proj)) return()

    pos <- cur_proj
    labels <- abbreviate(colnames(tour$data))
     
    qstrokeColor(painter) <- "grey50"
    qdrawSegment(painter, 0, 0, pos[, 1], pos[, 2])
    theta <- seq(0, 2 * pi, length = 50)
    qdrawLine(painter, cos(theta), sin(theta))
    
    r <- sqrt(rowSums(pos ^ 2))
    qdrawText(painter, labels[r > 0.1], pos[r > 0.1, 1], pos[r > 0.1, 2])
  }
  
  # ==================Controls==========================
  w <- gwindow("XY tour", visible = FALSE, 
    handler = function(...) {
      pause(TRUE)
      view$close()
    })
  vbox <- glayout(cont = w)

  # Variable selection column
  vbox[1, 1, anchor = c(-1, 0)] <- "Variable Selection"
  vbox[2, 1] <- Variables <- gcheckboxgroup(names(data[num]), 
    checked = TRUE, horizontal = FALSE, handler = update_tour)
    
  class_box <- ggroup(hor = F)
  add(class_box, glabel("Colour by"))
  add(class_box, Class <- gtable(c("None", names(data)[!num]), 
    multiple = TRUE), expand = TRUE)
  addhandlerclicked(Class, update_tour)
  vbox[5, 3, expand = TRUE] <- class_box
  
  # Tour selection column
  vbox[1, 2, anchor=c(-1, 0)] <- "Tour Type"
  tour_types <- c("Grand", "Little", "Guided(holes)", "Guided(cm)", "Guided(lda_pp)", "Local")
  vbox[2, 2] <- TourType <- gradio(tour_types, handler = update_tour, expand = T)


  # control aesthetics
  aes_box <- glayout()
  
  aes_box[1,1, anchor = c(1, -1)] <- "Speed"
  aes_box[1,2, expand = TRUE] <- sl <- 
    gslider(from = 0, to = 5, by = 0.1, value = 1)
  aes_box[2,1, anchor = c(1, -1)] <- "Transparency"
  aes_box[2,2, expand = TRUE] <- sl_alpha <- 
    gslider(from = 0.01, to = 1, by = 0.01, value = 1)
  aes_box[3,1, anchor = c(1, -1)] <- "Size"
  aes_box[3,2, expand = TRUE] <- sl_size <- 
    gslider(from = 0, to = 8, by = 0.5, value = 2)
  
  vbox[5, 1:2] <- aes_box

  # buttons control
  timer <- qtimer(30, step_tour)
  pause <- function(paused) {
    svalue(chk_pause) <- paused
    if (paused) {
      timer$stop()
    } else {
      timer$start()
    }
  }
  chk_pause <- gcheckbox("Pause",
    handler = function(h, ...) pause(svalue(h$obj)))
  vbox[1, 3] <- chk_pause
    
  buttonGroup <- ggroup(horizontal = F, cont=vbox)  
  glabel("Optimise for:", cont = buttonGroup)
  gradio(c("Quality", "Speed"), cont = buttonGroup, 
    handler = function(ev, ...) {
      view$setOpenGL(svalue(ev$obj) == "Speed")
    })
  vbox[2, 3, anchor = c(0, 1)] <- buttonGroup
  
  # Create canvas for displaying tour
  scene <- qscene()
  root <- qlayer(scene)

  points <- qlayer(root, render_tour)
  axes <- qlayer(root, render_axes)
  points$setCacheMode(Qt$QGraphicsItem$NoCache)
  axes$setCacheMode(Qt$QGraphicsItem$NoCache)
  points$setLimits(qrect(c(-1, 1), c(-1, 1)))
  axes$setLimits(qrect(c(-1, 1), c(-1, 1)))
  
  update_tour()
  pause(FALSE)
  visible(w) <- TRUE

  view <- qplotView(scene = scene, opengl = FALSE)
  print(view)
  
  invisible()
}


create_tour <- function(data, var_selected, cat_selected, axes_location, tour_type, aps, cur_proj) {
  if (length(var_selected) < 3) {
    gmessage("Please select at least three variables", icon = "warning")
    return()
  }
  
  # Work out point colours
  if (length(cat_selected) > 0  && cat_selected[1] != "None" ) {
    cat <- data[cat_selected]
    # collapse to single variable if multiple selected
    int <- interaction(cat, drop = TRUE)
    pal <- rainbow_hcl(length(levels(int)))
    col <- pal[as.numeric(int)]
  } else {
    col <- "black"
  }
  
  # Work out which type of tour to use
  tour <- switch(tour_type,
    "Grand" = grand_tour(), 
    "Little" = little_tour(), 
    "Guided(holes)" = guided_tour(holes), 
    "Guided(cm)" = guided_tour(cm), 
    "Guided(lda_pp)" = guided_tour(lda_pp(data[,cat_selected])),
    "Local" = local_tour(cur_proj)
  )
  
  
  sel <- data[var_selected]
  # Sphere the data if we're using a guided tour
  if (length(grep(tour_type, "Guided")) > 0) {
    sel <- sphere(sel)
  }
  
  list(
    data = rescale(sel),
    tour_path = tour,
    colour = col,
    aps = aps
  )
}
