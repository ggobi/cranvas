
constructCondition <- function (hdata) {
  require(reshape2)
  require(plyr)

  hdata$ID <- 1:nrow(hdata)
  res.melt <- melt(hdata,id.var="ID")
  res.melt$cond <- with(res.melt, sprintf("(%s == '%s')", variable, value))
  NAs <- which(is.na(res.melt$value))
  res.melt$cond[NAs] <- sprintf("is.na(%s)", res.melt$variable[NAs])
  condis <- ddply(res.melt, .(ID), summarize, condi = paste("(", paste(cond, collapse=" & "), ")"))
  paste(condis$condi, collapse="|")  
}

paste_formula <- function(form) {
# form has pieces wt, marg and cond
# output is character - needs to be converted to formula afterwards
	wtStr <- ""
	if (length(form$wt) > 0)
		wtStr <- form$wt[1]
	margStr <- "1"
	if (length(form$marg) > 0)
		margStr <- paste(form$marg,collapse="+")
	condStr <- ""
	if (length(form$cond) > 0)
		condStr <- paste("|", paste(form$cond, collapse="+"))

	formstring <- paste(wtStr,"~", margStr, condStr)
	return(formstring)
}


find_x_label <- function(form, divider) {
  parsed <- parse_product_formula(form)
  vars <- c(parsed$marg, parsed$cond)
  xlabs <- rev(vars[grep("h",divider)])
  paste(xlabs,"", collapse="+ ")
}

find_y_label <- function(form, divider) {
  parsed <- parse_product_formula(form)
  vars <- c(parsed$marg, parsed$cond)
  ylabs <- rev(vars[grep("v",divider)])
  paste(ylabs,"", collapse="+ ")  
}

settitle <- function(form) {
#  browser()
  if (!is.null(form))
    paste_formula(parse_product_formula(form))
}

extractVars <- function(form) {
  setdiff(unlist(parse_product_formula(form)), "1")
}
##' Mosaic plot.
##' Create a mosaicplot using a formula (as described in prodplot)
##'
##' Interactive elements for mosaic plots are arrow keys for navigating through the mosaic hierarchy:
##' arrow up reduces complexity of the mosaic by one variable, arrow down increases the complexity by one, if possible.
##' Arrow left and right rotate a previously included variable into the last split position.
##' Conditioning/Unconditioning is done with keys 'C' and 'U'
##' Keys 'B' and 'S" switch to bar and spine representation, respectively
##' Key 'R' rotates the last split variable between horizontal and vertical display.
##' 
##' @param data a mutaframe which is typically built upon a data frame
##' along with several row attributes
##' @param formula a formula to describe order in which variables go into the mosaicplot. The first variables are the ones visually most important, i.e. Y ~ X1 + X2 + X3 first splits according to X3, then according to X2, then according to X1
##' @param divider structure of the split in each direction. Choices are "hbar", "vbar" for horizontal/vertical barcharts, "hspine", "vspine" for horizontal/vertical spineplots.
##' @param cascade parameter for prodplot in package productplots
##' @param scale_max parameter for prodplot in package productplots
##' @param na.rm handling of missing values, defaults to FALSE
##' @param subset parameter for prodplot - 
##' @param colour fill colour of rectangles - only used if colour is not used in the data
##' @param main parameter for prodplot
##' @param ...
##' @return NULL
##' @author Heike Hofmann
##' @export
##' @example inst/examples/qmosaic-ex.R
qmosaic <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE, subset=NULL, colour="grey30", main=NULL, ...) {
    data = check_data(data)
    b = brush(data)
    b$select.only = TRUE; b$draw.brush = FALSE  # a selection brush
		z = as.list(match.call()[-1])
    s = attr(data, 'Scales')
    var = extractVars(z$formula)

    redoHiliting <- FALSE
    redoColor <- FALSE
    meta =
        Mosaic.meta$new(var=var, form = as.formula(z$formula), origForm=as.formula(z$formula),
                     xlim=c(0,1), ylim=c(0,1), alpha = 1, 
                     inactiveVar=NULL, inactiveDivider=NULL,
                     active = TRUE, main=settitle(z$formula), ylab="", xlab="", main=main)
    if(is.null(divider)) divider = mosaic()
    if (!is.character(divider)) {
      form = parse_product_formula(z$formula)
      splits = c(form$marg, form$cond)
      divider = divider(length(splits))
    }
    meta$divider = divider
    meta$origDivider = meta$divider

    recalcColor = function() {
      redoColor <<-FALSE
      idx = visible(data) 
      if (sum(idx) > 0) {
        
        df <- data.frame(data[idx,])
        
        form <- parse_product_formula(meta$form)
        df$wt <- 1
        if (length(form$wt) == 1) df$wt <- df[,form$wt]
        var <- unlist(c(form$marg, form$cond))
        
        cols <- ddply(df, var, function(x) {
          dc <- xtabs(wt~.color, data=x, exclude=NULL)/sum(x$wt)
          dc[is.nan(dc)] <- 0
          dc
        })
        require(reshape2)
        cm <- melt(cols, id.var=var)              
        names(cm) <- gsub("variable", ".color",names(cm))        
        names(cm) <- gsub("value", "cval",names(cm))        
        
        colID <- grep(".color", names(meta$mdata))
        if (length(colID) >0) meta$mdata <- meta$mdata[-colID]
        meta$cdata <- merge(meta$mdata, cm, by=var)
## set order of the colors here
#        browser()
        meta$cdata$cid <- as.numeric(meta$cdata$.color)
        meta$cdata <- ddply(meta$cdata, var, transform, 
                            cval=cumsum(cval[order(cid)]), 
                            .color=.color[order(cid)])
      } else {
        meta$cdata <- meta$mdata  
        meta$cid <- as.numeric(meta$cdata$.color)
        meta$cdata$cval <- 0
      }

      split <- meta$divider[1]
      if (length(grep("v", split))>0) split <- "hspine"
      else split <- "vspine"

      if (split =="vspine") {
        meta$cdata$t =  with(meta$cdata, b + (t-b)*cval)
        meta$cdata <- ddply(meta$cdata, var, transform, 
                            b = c(b[1], t[-length(t)]))     
      } else {
       meta$cdata$r =  with(meta$cdata, l + (r-l)*cval)
       meta$cdata <- ddply(meta$cdata, var, transform, 
                           l = c(l[1], r[-length(r)]))     
      }
    }
    
    recalcHiliting = function() {
      redoHiliting <<-FALSE
      idx = visible(data) 
      if (sum(idx) > 0) {
      
        df <- data.frame(data[idx,])
        
        form <- parse_product_formula(meta$form)
        df$wt <- 1
        if (length(form$wt) == 1) df$wt <- df[,form$wt]
      
        var <- unlist(c(form$marg, form$cond))
        hils <- ddply(df, var, summarize, hilited = sum(wt[.brushed])/sum(wt))
        hils$hilited[is.nan(hils$hilited)] <- 0

        hilID <- grep("hilited", names(meta$mdata))
        if (length(hilID) >0) meta$mdata <- meta$mdata[-hilID]
        meta$hdata <- merge(meta$mdata, hils, by=var)
      } else {
        meta$hdata <- meta$mdata  
        meta$hdata$hilited <- 0
      }
      
      split <- meta$divider[1]
      if (length(grep("v", split))>0) split <- "hspine"
      else split <- "vspine"
      
      if (split =="vspine") {
        meta$hdata$t =  with(meta$hdata, b + (t-b)*hilited)
      } else
        meta$hdata$r =  with(meta$hdata, l + (r-l)*hilited)
    }

    setylab <- function() {
      parsed <- parse_product_formula(meta$form)
      vars <- c(parsed$marg, parsed$cond)
      
      yvars <- rev(vars[grep("v",meta$divider)])
      meta$yat = seq(0,1, length=5)
      meta$ylabels = round(seq(0,1, length=5),2)
      if (length(yvars)>=1) {
        yvar <- yvars[1]
        df <- subset(meta$mdata, l==0)
        at <- ddply(df, yvar, summarize, yat=(min(b)+max(t))/2)
        meta$yat = at$yat
        meta$ylabels = at[,1]
      }           
    }
    
    setxlab <- function() {
      parsed <- parse_product_formula(meta$form)
      vars <- c(parsed$marg, parsed$cond)
      xvars <- rev(vars[grep("h",meta$divider)])
      meta$xat = seq(0,1, length=5)
      meta$xlabels = round(seq(0,1, length=5), 2)
      if (length(xvars)>=1) {
        xvar <- xvars[1]
        #      browser()
        df <- subset(meta$mdata, b==0)
        at <- ddply(df, xvar, summarize, xat=(min(l)+max(r))/2)
        meta$xat = at$xat
        meta$xlabels = at[,1]
      }      
    }
    
    recalc = function() {
      idx = visible(data)
      df <- data.frame(data[idx,])
      mdata <- prodcalc(df, meta$form, meta$divider, cascade, scale_max, na.rm = na.rm)
      meta$mdata <- subset(mdata, level==max(mdata$level), drop=FALSE)
      meta$xlab <- find_x_label(meta$form, meta$divider)
      meta$ylab <- find_y_label(meta$form, meta$divider)
      setxlab()
      setylab()
      recalcColor()
      recalcHiliting()
    }
 
    
    compute_coords = function() {
			meta$limits = extend_ranges(cbind(meta$xlim, meta$ylim))
      meta$minor = "xy"
      recalc()
    }
    compute_coords()

    recalcColor()

		removeSplit = function() {
			form = parse_product_formula(meta$form)			
      if (length(form$marg) > 1) {
      	meta$inactiveVar <- c(form$marg[1], meta$inactiveVar)
      	meta$inactiveDivider <- c(meta$divider[1], meta$inactiveDivider)
        form$marg <- form$marg[-1]
        meta$divider <- meta$divider[-1]        
      } 
      else return()
#         if (length(form$marg) == 1) {
#         if (form$marg[1] == "1") return()
#         else {
#       	  meta$inactiveVar <- c(form$marg[1], meta$inactiveVar)
#           form$marg[1] = "1"
#         }
#       
#         }

      meta$form <- as.formula(paste_formula(form))
			recalc()
			layer.main$invalidateIndex()
			qupdate(layer.main)
		}

		addSplit = function() {
			form = parse_product_formula(meta$form)
			if(length(meta$inactiveVar) < 1) return()
      
      if ((length(form$marg) == 0) | (form$marg[1] == "1")) {
				form$marg[1] <- meta$inactiveVar[1]
				meta$inactiveVar <- meta$inactiveVar[-1]
			} else {
				form$marg <- c( meta$inactiveVar[1], form$marg)
				meta$inactiveVar <- meta$inactiveVar[-1]
				lastSplit <- length(form$marg)
				meta$divider <- c(meta$inactiveDivider[1], meta$divider)
				meta$inactiveDivider = meta$inactiveDivider[-1]
			}

			meta$form <- as.formula(paste_formula(form))
			recalc()
			layer.main$invalidateIndex()
			qupdate(layer.main)	
		}

		rotateLeft = function() {
			form = parse_product_formula(meta$form)
			if(length(meta$inactiveVar) < 1) return()
      
      if ((length(form$marg) == 0) | (form$marg[1] == "1")) {
				form$marg[1] <- meta$inactiveVar[1]
				meta$inactiveVar <- meta$inactiveVar[-1]
			} else {
				save <- form$marg[1]
				form$marg[1] <- meta$inactiveVar[1]
				meta$inactiveVar <- c(meta$inactiveVar[-1], save)
			}
			meta$form <- as.formula(paste_formula(form))
			recalc()
			layer.main$invalidateIndex()
			qupdate(layer.main)	
		}

		rotateRight = function() {
			form = parse_product_formula(meta$form)
			if(length(meta$inactiveVar) < 1) return()
      
      if ((length(form$marg) == 0) | (form$marg[1] == "1")) {
				form$marg[1] <- meta$inactiveVar[1]
				meta$inactiveVar <- meta$inactiveVar[-1]
			} else {
				save <- form$marg[1]
				lastInactive <- length(meta$inactiveVar)
				form$marg[1] <- meta$inactiveVar[lastInactive]
				meta$inactiveVar <- c(save, meta$inactiveVar[-lastInactive])
			}
			meta$form <- as.formula(paste_formula(form))
			recalc()
			layer.main$invalidateIndex()
			qupdate(layer.main)	
		}
		
		rotateSplit = function() {
		  if (length(grep("v", meta$divider[1])) > 0) 
         meta$divider[1] <- gsub("v", "h", meta$divider[1])
      else
         meta$divider[1] <- gsub("h", "v", meta$divider[1])
			
			recalc()
		  layer.main$invalidateIndex()
			qupdate(layer.main)
		}

		unconditionVar = function() {
			form = parse_product_formula(meta$form)

			if (length(form$cond) < 1) return()
			
			# take last conditioning variable and move in as first split
			form$marg <- c(form$marg, form$cond[1])
			form$cond <- form$cond[-1]
			
			meta$form <- as.formula(paste_formula(form))
			recalc()
			layer.main$invalidateIndex()
			qupdate(layer.main)
		}

		conditionVar = function() {
			form = parse_product_formula(meta$form)

			if (length(form$marg) < 1) return()
			
			# take fist split and condition on it
			firstSplit <- length(form$marg)
			form$cond <- c(form$marg[firstSplit], form$cond)
			form$marg <- form$marg[-firstSplit]
			
			meta$form <- as.formula(paste_formula(form))
			recalc()
			layer.main$invalidateIndex()
			qupdate(layer.main)
		}

    meta$brush.size = c(1, -1) * apply(meta$limits, 2, diff) / 15
    
    main_draw = function(layer, painter) {
			if (redoColor) recalcColor()
			color <- "grey30"
      
			with(meta$mdata, qdrawRect(painter,l,b,r,t, fill="white", stroke=color))
			with(meta$cdata, qdrawRect(painter,l,b,r,t, fill=as.character(.color), stroke=as.character(.color)))
      
      zeroes <- subset(meta$mdata, .wt==0, drop=FALSE)
      if (nrow(zeroes) > 0) {
        qdrawCircle(painter, zeroes$l, zeroes$b, r = 3,
          stroke = color, fill = "white")
      }
    }
    brush_draw = function(layer, painter) {
      if (redoHiliting) recalcHiliting()

      color <- b$color
      
      with(meta$hdata, qdrawRect(painter,l,b,r,t, fill=color, stroke=color))
      draw_brush(layer, painter, data, meta)
    }
    brush_mouse_press = function(layer, event) {
        common_mouse_press(layer, event, data, meta)
    }
    brush_mouse_move = function(layer, event) {
        rect = qrect(update_brush_size(meta, event))
        hits = layer$locate(rect)
        hits <- hits[hits < nrow(meta$mdata)]
        
        if (length(hits)) {
          ## rectangles are drawn in the same order as in mdata
 #         print(hits)
          form <- parse_product_formula(meta$form)
          var <- unlist(c(form$marg, form$cond))
          selected <- meta$mdata[hits+1, var, drop=FALSE]
          condstr = constructCondition(selected)
          hits = with(data.frame(data), which(eval(parse(text=condstr))))
        }
        selected(data) = mode_selection(selected(data), hits, mode = b$mode)
        common_mouse_move(layer, event, data, meta)
    }
    brush_mouse_release = function(layer, event) {
        brush_mouse_move(layer, event)
        common_mouse_release(layer, event, data, meta)
    }
    key_press = function(layer, event) {
        common_key_press(layer, event, data, meta)

				key <- event$key()
				
				if (key == Qt$Qt$Key_Up) {        # arrow up
					removeSplit()
				} 
				if (key == Qt$Qt$Key_Down) {        # arrow down
					addSplit()
				}       
				if (key == Qt$Qt$Key_Right) {        # arrow right
					rotateRight()
				}       
				if (key == Qt$Qt$Key_Left) {        # arrow right
					rotateLeft()
				}       
				if (key == Qt$Qt$Key_R) {     # 'r' or 'R' for 'rotate'
					rotateSplit()
        }
				if (key == Qt$Qt$Key_U) {     # 'u' or 'U' for 'uncondition'
					unconditionVar()
        }
				if (key == Qt$Qt$Key_C) {     # 'c' or 'C' for 'condition'
					conditionVar()
        }
        if (key == Qt$Qt$Key_B) {     # 'b' or 'B' for 'spine to Bar'
          firstletter <- substr(meta$divider[1],1,1)
          meta$divider[1] <- sprintf("%sbar", firstletter)
          recalc()
          layer.main$invalidateIndex()
          qupdate(layer.main)
        }
        if (key == Qt$Qt$Key_S) {     # 's' or 'S' for 'bar to Spine'
          firstletter <- substr(meta$divider[1],1,1)
          meta$divider[1] <- sprintf("%sspine", firstletter)
          recalc()
          layer.main$invalidateIndex(); qupdate(layer.main)
        }
        
    }
    key_release = function(layer, event) {
        common_key_release(layer, event, data, meta)
    }
    identify_hover = function(layer, event) {
        if (!b$identify) return()
        b$cursor = 2L
        meta$pos = as.numeric(event$pos())
        meta$identified = layer$locate(identify_rect(meta))
        qupdate(layer.identify)
    }
    identify_draw = function(layer, painter) {
      if (!b$identify || !length(idx <- meta$identified)) return()

      idx <- idx[idx <= nrow(meta$mdata)]
      if (length(idx) == 0) return()

      idx = idx + 1
      form <- parse_product_formula(meta$form)
      var <- rev(unlist(c(form$marg, form$cond)))
      for(i in var)
        meta$mdata[,i] <- as.character(meta$mdata[,i])
      id <- paste(var, meta$mdata[idx, var], sep=": ", collapse="\n")

      sumwt <- sum(meta$mdata[, ".wt"])  
      ivals <- paste(sprintf("\ncount: %s\nproportion: %.2f%%", 
                             meta$mdata[idx, ".wt"], 
                             meta$mdata[idx, ".wt"]/sumwt*100), collapse="\n")
            
      meta$identify.labels = paste(id, ivals, collapse="")
        
      draw_identify(layer, painter, data, meta)
      qdrawRect(painter, meta$mdata$l[idx], meta$mdata$b[idx], meta$mdata$r[idx],
                meta$mdata$t[idx], stroke = b$color, fill = NA)
    }
    
    
    
    scene = qscene()
    layer.root = qlayer(scene)
    layer.main =
        qlayer(paintFun = main_draw,
               mousePressFun = brush_mouse_press, mouseReleaseFun = brush_mouse_release,
               mouseMove = brush_mouse_move, hoverMoveFun = identify_hover,
               keyPressFun = key_press, keyReleaseFun = key_release,
               focusInFun = function(layer, event) {
                   common_focus_in(layer, event, data, meta)
               }, focusOutFun = function(layer, event) {
                   common_focus_out(layer, event, data, meta)
               },
               limits = qrect(meta$limits))
    layer.brush = qlayer(paintFun = brush_draw, limits = qrect(meta$limits))
    layer.identify = qlayer(paintFun = identify_draw, limits = qrect(meta$limits))
    layer.title = qmtext(meta = meta, side = 3)
    layer.xlab = qmtext(meta = meta, side = 1)
    layer.ylab = qmtext(meta = meta, side = 2)
    layer.xaxis = qaxis(meta = meta, side = 1)
    layer.yaxis = qaxis(meta = meta, side = 2)
    layer.grid = qgrid(meta = meta)
    layer.keys = key_layer(meta)
    layer.root[0, 2] = layer.title
    layer.root[2, 2] = layer.xaxis
    layer.root[3, 2] = layer.xlab
    layer.root[1, 1] = layer.yaxis
    layer.root[1, 0] = layer.ylab
    layer.root[1, 2] = layer.grid
    layer.root[1, 2] = layer.main
    layer.root[1, 2] = layer.brush
    layer.root[1, 2] = layer.keys
    layer.root[1, 2] = layer.identify
    layer.root[1, 3] = qlayer()

    set_layout = function() {
        fix_dimension(layer.root,
                      row = list(id = c(0, 2, 3), value = c(prefer_height(meta$main),
                                                  prefer_height(meta$xlabels),
                                                  prefer_height(meta$xlab))),
                      column = list(id = c(1, 0, 3), value = c(prefer_width(meta$ylabels),
                                                     prefer_width(meta$ylab, FALSE),
                                                     10)))
    }
    set_layout()
    meta$mainChanged$connect(set_layout)
    meta$xlabChanged$connect(set_layout); meta$ylabChanged$connect(set_layout)
    meta$xlabelsChanged$connect(set_layout); meta$ylabelsChanged$connect(set_layout)

    view = qplotView(scene = scene)
    view$setWindowTitle(sprintf('Mosaic plot: %s', meta$main))

    
    meta$xlabChanged$connect(setxlab)    
    meta$ylabChanged$connect(setylab)    
    
    meta$formChanged$connect(function() {
        meta$main = settitle(meta$form)
        view$setWindowTitle(sprintf('Mosaic plot: %s', meta$main))
    })
    d.idx = add_listener(data, function(i, j) {
        switch(j, .brushed = { redoHiliting <<-TRUE; qupdate(layer.main)},
               .color = {
                 redoColor <<-TRUE;
                   qupdate(layer.main)
               }, {
                   compute_coords(); redoHiliting<<- TRUE
                   redoColor <<- TRUE; flip_coords()
                   layer.main$invalidateIndex()
                   qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis)
                   qupdate(layer.main)
               })
    })
    qconnect(layer.main, 'destroyed', function(x) {
        ## b$colorChanged$disconnect(b.idx)
        remove_listener(data, d.idx)
    })

    b$cursorChanged$connect(function() {
        set_cursor(view, b$cursor)
    })
    sync_limits(meta, layer.main, layer.brush, layer.identify)
    meta$manual.brush = function(pos) {
        brush_mouse_move(layer = layer.main, event = list(pos = function() pos))
    }
    attr(view, 'meta') = meta
    layerList <- LayerList(layer.root = layer.root)
    res <- CranvasPlot(layerList, scene = scene, view = view, meta = meta, data = data)
    res
}


Mosaic.meta =
    setRefClass("Mosaic_meta", contains = "CommonMeta",
                fields = properties(list(
                  var = 'character',
                  form='formula',
                  divider='character',
                  origForm='formula',
                  origDivider='character',
                  inactiveVar='character',
                  inactiveDivider='character',
                  mdata='data.frame',
                  hdata='data.frame',
                  cdata='data.frame'
                    )))
