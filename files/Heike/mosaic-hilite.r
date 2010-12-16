rm(hbar)
rm(vbar)
source("labels.r")
require(stringr)
require(productplots)

require(plumbr)

id <- function(x) return(x)

# assume that data is mutaframe
qmosaic <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE, subset=NULL, colour="grey30", main=NULL, ...) {
  ## check if an attribute exist
  has_attr = function(attr) {
      attr %in% names(data)
  }

  ## parameters for the brush
  .brush.attr = attr(data, '.brush.attr')

#  row.attr <- get_row_attr(data)
  if (!has_attr('.brushed')) data$.brushed = FALSE

  odata <- data
  df <- data.frame(odata)
#  df.brushed <- factor(df.brushed, levels=c("FALSE","TRUE"))
#print(formula)
  data <- prodcalc(df, formula, divider, cascade, scale_max, na.rm = na.rm)
  if (is.null(data$.brushed)) data$.brushed <- FALSE

  .level <- max(data$level)-1

  extract.formula <- function(formula) {
    form <- parse_product_formula(formula)

    ncond <- length(form$cond)
    nmarg <- length(form$marg)

    if (ncond <= .level) {
        fcond <- form$cond
        .level <- .level - ncond
    } else {
        fcond <- form$cond[(ncond-.level+1):ncond]
        .level <- 0
    }

    if (.level == 0) fmarg <- character(0)
    else {
      if (nmarg <= .level) {
        fmarg <- form$marg
      } else {
        fmarg <- form$marg[(nmarg-.level+1):nmarg]
      }
    }
    # piece everything together
    formstring <- paste(form$wt,"~ ")

    if (length(fmarg) > 0) {
      formstring <- paste(formstring, paste(fmarg, collapse= "+"))
    } else {
      formstring <- paste(formstring,"1")
    }

    if (length(fcond) > 0) {
      formstring <- paste(formstring, "|", paste(fcond, collapse= "+"))
    }
    .activevars <<- c(fmarg, fcond)

    return(formstring)
  }


  setuphilite <- function(formula) {
 #   if (is.null(row.attr$.brushed)) row.attr$.brushed <- rep(FALSE, nrow(odata))

    formulahil <- NULL
    dividerhil <- NULL

    if (sum(odata$.brushed, na.rm=T) > 0) {
      #  browser()
      form <- parse_product_formula(formula)
      fmarg <- c(".brushed", form$marg)
      fcond <- form$cond

      formstring <- paste(form$wt,"~ ")

      if (length(fmarg) > 0) {
        formstring <- paste(formstring, paste(fmarg, collapse= "+"))
      } else {
        formstring <- paste(formstring,"1")
      }

      if (length(fcond) > 0) {
        formstring <- paste(formstring, "|", paste(fcond, collapse= "+"))
      }

      formulahil <- as.formula(formstring)

      dvd <- rev(rev(divider)[1:.level])
      if (dvd[1] %in% c("hspine", "hbar")) dividerhil <- c("vspine",dvd)
      else if (dvd[1] %in% c("vspine", "vbar")) dividerhil <- c("hspine",dvd)
    }
    return(list(formulahil, dividerhil))
  }

  #  hils <- setuphilite(formula=as.formula(extract.formula(formula)))
  #  formulahil <<- hils[[1]]
  #  dividerhil <<- hils[[2]]

  #  datahil <- prodcalc(data.frame(odata), formulahil, dividerhil, cascade, scale_max, na.rm = na.rm)
# set up a hiliting structure with 0 rows:
	datahil <- data.frame(data[1,])
	datahil <- datahil[-1,]

  #browser()

  if (!missing(subset)) {
    sel <- eval(substitute(subset), data, parent.frame())
    data <- data[sel & !is.na(sel), ]
  }

  .bgcolor<-"grey80"
  .level <- max(data$level)

  .df.title <- FALSE
  .clevel <- 0
  form <- parse_product_formula(formula)
  .activevars <- c(form$marg, form$cond)

  top <- data$t
  bottom <- data$b
  left <- data$l
  right <- data$r

  if (is.null(main)) .df.title <- TRUE
  xlab <- find_x_label(data)
  ylab <- find_y_label(data)

  dataRanges <- c(
    make_data_ranges(c(min(left), max(right))),
    make_data_ranges(c(min(bottom),max(top))))
  #  bprint(dataRanges)

  # space in window around plot (margins in base R)
  # this space depends on the labels needed on the left
  # find out about these first:

  row <- find_row_level(data)
  ylabels <- NULL
  if (!is.na(row))
      ylabels <- row_labels(data[data$level == row, ])

  if (.df.title) {
    main <- as.character(formula)
  }
  windowRanges <- make_window_ranges(dataRanges, xlab, ylab,
    ytickmarks=ylabels, main=main)

  lims <- qrect(windowRanges[c(1,2)], windowRanges[c(3,4)])

  coords <- function(item, painter, exposed) {
    sx <- scale_x_product(data)
    sy <- scale_y_product(data)

    # grey background with grid lines
    draw_grid_with_positions_fun(painter, dataRanges, sx$breaks, sy$breaks,
       sx$minor_breaks, sy$minor_breaks)

    # put labels, if appropriate
    col <- find_col_level(data)
    if (!is.na(col)) {
      labels <- col_labels(data[data$level == col, ])

      draw_x_axes_with_labels_fun(painter, dataRanges,
          axisLabel=labels$label, labelHoriPos=labels$pos, name=xlab)
    } else {
      draw_x_axes_with_labels_fun(painter, dataRanges,
        axisLabel=rep("",length(sx$breaks)), labelHoriPos=sx$breaks,
        name=xlab)
    }

    if (!is.na(row)) {
      labels <- row_labels(data[data$level == row, ])
      draw_y_axes_with_labels_fun(painter, dataRanges,
        axisLabel=labels$label, labelVertPos=labels$pos, name=ylab)
    } else {
      draw_y_axes_with_labels_fun(painter, dataRanges,
         axisLabel=rep("",length(sy$breaks)), labelVertPos=sy$breaks,
         name=ylab)
    }
  }

  mosaic.all <- function(item, painter, exposed) {
    all.data <- subset(data, level==.level)

    top <- all.data$t
    bottom <- all.data$b
    left <- all.data$l
    right <- all.data$r

    qdrawRect(painter,
      left,
      bottom,
      right,
      top,
      fill=colour)

    if (.df.title) {
      add_title_fun(painter, dataRanges, title=extract.formula(formula))
    }
  }

  # Brushing -----------------------------------------------------------------
  .startBrush <- NULL
  .endBrush <- NULL
  .brush <- FALSE

  drawBrush <- function(item, painter, exposed) {
    left = min(.startBrush[1], .endBrush[1])
    right = max(.startBrush[1], .endBrush[1])
    top = max(.startBrush[2], .endBrush[2])
    bottom = min(.startBrush[2], .endBrush[2])

    qdrawRect(painter, left, bottom, right, top,
      fill=rgb(0,0,0,alpha=0.3), stroke="black")
  }

  recalchiliting <- function() {
    hils <- setuphilite(formula=as.formula(extract.formula(formula)))

    formulahil <<- hils[[1]]
    dividerhil <<- hils[[2]]
	df <- data.frame(odata)



    df$.brushed <- factor(df$.brushed, levels=c("TRUE","FALSE"))

    if (!is.null(formulahil))
    	datahil <<- prodcalc(df, formulahil, dividerhil, cascade,
      scale_max, na.rm = na.rm)
    else if (nrow(datahil)>0) datahil <<- datahil[-(1:nrow(datahil)),]
  }

  brushing_draw <- function(item, painter, exposed, ...) {
    if (TRUE) {
      if (.brush) {
#		if (is.null(data$.brushed))
#          data$.brushed <- FALSE
      	hdata <- subset(data, (.brushed==TRUE) & (level == (.level)))
      } else {
	    recalchiliting()
	#	if (is.null(datahil$.brushed)) datahil$.brushed <- FALSE
      	hdata <- subset(datahil, (.brushed==TRUE) & (level == (.level+1)))
      }

      if (nrow(hdata)>0) {

        top <- hdata$t
        bottom <- hdata$b
        left <- hdata$l
        right <- hdata$r

#  .brush.attr = attr(odata, '.brush.attr')

		brushcolor <- brush_attr(data, ".brushed.color")
        qdrawRect(painter, left, bottom, right, top, fill=brushcolor)
      }
    }

    if (!is.null(.endBrush)) {
      drawBrush(item, painter, exposed)
    }
  }

  brushing_mouse_press <- function(item, event, ...) {
    .brush <<- TRUE
    if (is.null(.startBrush)) {
      .startBrush <<- as.numeric(event$pos())
    }
    qupdate(brushing_layer)
  }

  brushing_mouse_move <- function(item, event, ...) {
    .endBrush <<- as.numeric(event$pos())

    setHiliting()
    qupdate(brushing_layer)
  }

  brushing_mouse_release <- function(item, event, ...) {
    .endBrush <<- as.numeric(event$pos())
    setHiliting()
    qupdate(brushing_layer)


    .brush <<- FALSE


    .startBrush <<- NULL
    .endBrush <<- NULL

	 setSelected()

#    print("changed?")
#    print(summary(row.attr$.brushed))

#      recalchiliting()
#	  qupdate(brushing_layer)
  }

  setHiliting <- function() {
    left = min(.startBrush[1], .endBrush[1])
    right = max(.startBrush[1], .endBrush[1])
    top = max(.startBrush[2], .endBrush[2])
    bottom = min(.startBrush[2], .endBrush[2])

    data$.brushed <<- (data$level == .level) & (data$l <= right) &
      (data$r >= left) & (data$b <= top) & (data$t >= bottom)
  }

  setSelected <- function() {
    hdata <- subset(data, (.brushed==TRUE) & (level == .level), drop=FALSE)[,.activevars, drop=FALSE]
	if (nrow(hdata) > 0) {
		hdata$ID <- 1:nrow(hdata)
		res.melt <- melt(hdata,id.var="ID")
		res.cond <- adply(res.melt, 1, function(x) {
			if (is.na(x$value)) cstr <- paste("is.na(",x$variable,")", sep="")
			else cstr <- paste("(",x$variable,"=='",x$value,"')",sep="")
			return(cond=cstr)
		})
		res.cond <- res.cond[,-3]
		names(res.cond)[3] <- "value"
		cast.res <- cast(res.cond, ID~., function(x) return(paste(x, collapse=" & ")))

		cond1 <- paste("(",cast.res[,2],")", sep="",collapse=" | ")
		idx <- with(data.frame(odata), which(eval(parse(text=cond1))))

		.brushed <- rep(FALSE, nrow(odata))
		if (length(idx)) .brushed[idx] <- TRUE

		odata$.brushed <- .brushed
	} else {
		odata$.brushed <- FALSE
	}
 #   idx <- with(data.frame(odata), which(eval(parse(text=cond1))))
 #   return(idx)
  }


  # Display category information on hover (query) ----------------------------
  .queryPos <- NULL

  query_draw <- function(item, painter, exposed, ...) {
    # Don't draw when brushing
    if (.brush) return()
    if (is.null(.queryPos)) return()

    x <- .queryPos[1]
    y <- .queryPos[2]

    info <- subset(data, (y <= t) & (y >= b) & (x <= r) & (x >=l) &
      (level == .level))

    # Nothing under mouse
    if (nrow(info) == 0) return()

    # Work out label text
    idx <- setdiff(names(data),c("l","t","r","b", ".wt","level",
      ".brushed"))[1:.level]
    infodata <- as.character(unlist(info[1,idx]))
    infostring <- paste(idx, infodata,collapse="\n", sep=":")

    qstrokeColor(painter) <- "white"
    qdrawText(painter, infostring, x, y, valign="top", halign="left")
  }

  query_hover <- function(item, event, ...) {
    if (.brush) return()

    .queryPos <<- as.numeric(event$pos())
    qupdate(querylayer)
  }

  query_hover_leave <- function(item, event, ...) {
    .queryPos <<- NULL
    qupdate(querylayer)
  }

  # Key board events ---------------------------------------------------------

  keyPressFun <- function(item, event, ...) {
    # print(event$key())
    key <- event$key()

    datachanged <- FALSE
    formulachanged <- FALSE
    form <- parse_product_formula(formula)

    if (key == Qt$Qt$Key_Up) {        # arrow up
      if (.level > 1) {
        .level <<- .level - 1
      }
    } else if (key == Qt$Qt$Key_Down) {        # arrow down
      if (.level < max(data$level)) {
        .level <<- .level + 1
      }
    } else if (key == Qt$Qt$Key_Left) {        # arrow left
    # move variable into mosaic plot from left
      if (.level < max(data$level)) {
        lindx <- max(data$level)-.level+1

        marg_n <- length(form$marg)
        vars <- c(form$marg, form$cond)
        vars <- rev(c(rev(vars[-lindx]),vars[lindx]))
        form$marg <- vars[1:marg_n]
        form$cond <- setdiff(vars, form$marg)

        formulachanged <- TRUE
      }
    } else if (key == Qt$Qt$Key_C) {     # 'c' or 'C' for 'condition'
      if (.clevel < max(data$level)) {
          .clevel <<- .clevel+1

        vars <- c(form$marg, form$cond)
        form$cond <- rev(rev(vars)[1:.clevel])
        form$marg <- setdiff(vars, form$cond)
        formulachanged <- TRUE
      }
    } else if (key == Qt$Qt$Key_U) {     # 'u' or 'U' for 'unconditioning'
    #  if (lindx < max(data$level)) {
      if (.clevel > 0) {
          .clevel <<- .clevel-1
        vars <- c(form$marg, form$cond)
        if (.clevel == 0) {
            form$cond <- character(0)
            form$marg <- vars
        } else {
            form$cond <- rev(rev(vars)[1:.clevel])
            form$marg <- setdiff(vars, form$cond)
        }

        formulachanged <- TRUE
      }
    } else if (key == Qt$Qt$Key_R) {     # 'r' or 'R' for 'rotate'
        lindx <- max(data$level)-.level + 1

      if (divider[lindx] %in% c('hbar','vbar'))
          divider[lindx] <<- setdiff(c('hbar','vbar'),divider[lindx])
      if (divider[lindx] %in% c('hspine','vspine'))
          divider[lindx] <<- setdiff(c('hspine','vspine'),divider[lindx])

        datachanged <- TRUE

#browser()
    } else if (key == Qt$Qt$Key_S) {     # 's' or 'S' for 'spine'
            lindx <- max(data$level)-.level + 1

      if (divider[lindx] == 'vbar')
          divider[lindx] <<- 'vspine'
      if (divider[lindx] == 'hbar')
          divider[lindx] <<- 'hspine'

       datachanged <- TRUE
    } else if (key == Qt$Qt$Key_B) {     # 'b' or 'B' for 'bar'
            lindx <- max(data$level)-.level + 1

      if (divider[lindx] == 'vspine')
          divider[lindx] <<- 'vbar'
      if (divider[lindx] == 'hspine')
          divider[lindx] <<- 'hbar'

        datachanged <- TRUE
    }

    if (formulachanged) {
        formstring <- paste(form$wt,"~ ")

        if (length(form$marg) > 0) {
		  formstring <- paste(formstring, paste(form$marg, collapse= "+"))
        } else {
          formstring <- paste(formstring,"1")
        }

        if (length(form$cond) > 0) {
          formstring <- paste(formstring, "|", paste(form$cond,
            collapse= "+"))
        }
        formula <<- as.formula(formstring)
        datachanged <- TRUE
    }

    if (datachanged) {

	  df <- data.frame(odata)
##	  df$.brushed <- row.attr$.brushed
      data <<- prodcalc(df, formula, divider, cascade, scale_max,
        na.rm = na.rm)
    }

    # should be updating the data set, then start all fresh ...
    # need to figure out how to properly deal with hiliting of parts of the
    # boxes

    qupdate(bglayer)
    qupdate(datalayer)
    qupdate(brushing_layer)
  }

  scene = qscene()

  bglayer = qlayer(scene, coords, limits = lims, clip = FALSE,
    keyPressFun=keyPressFun)
  datalayer = qlayer(scene, mosaic.all, limits = lims, clip = FALSE)
  brushing_layer = qlayer(scene, brushing_draw,
    mousePressFun = brushing_mouse_press, mouseMoveFun = brushing_mouse_move,
    mouseReleaseFun = brushing_mouse_release,
    limits = lims, clip = FALSE)
  querylayer = qlayer(scene, query_draw, limits = lims, clip = FALSE,
    hoverMoveFun = query_hover, hoverLeaveFun = query_hover_leave)

  # update the brush layer in case of any modifications to the mutaframe
  if (is.mutaframe(odata)) {
	add_listener(odata, function(i,j) {
	  if (j == ".brushed") {
	  	qupdate(brushing_layer)
	  }
	})
  }
  add_listener(.brush.attr, function(i, j) {
# wouldn't need to call recalchiliting ...
    qupdate(brushing_layer)
  })

  qplotView(scene = scene)
}

