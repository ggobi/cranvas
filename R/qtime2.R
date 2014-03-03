qtime2 = function(time, y, data, group=NULL,
                  shift=c(1,4,7,12,24), alpha=1, size=2, asp=NULL, 
                  series.stats=TRUE, fun.base=min,
                  main=NULL, xlab=NULL, ylab=NULL,...){
  
  #####################
  ## data processing ##----------
  #####################
  
  data = check_data(data)
  call = as.list(match.call()[-1])
  time = as.character(call$time)
  y = as.character(call$y)
  if(y[1] == "c") y = y[-1]
  tdata = time_qdata2(data, y, c(time,as.character(call$group)))
  b = brush(tdata)
  meta = Time.meta2$new(varname = list(x = time, y = y), minor = 'xy')
  time_meta_initialize2(meta, call, data=tdata, shift=shift, 
                       alpha=alpha, size=size, asp=asp,
                       main=main, xlab=xlab, ylab=ylab)
  if ('g' %in% names(meta$varname) & series.stats) series.stats = FALSE
  meta$active = TRUE
  tree = createTree(data.frame(x=meta$data$xtmp,y=meta$data$ytmp))
  update_meta_group(meta)
  update_meta_xwrap_color(meta,tdata)
  compute_area(meta,tdata,fun.base)
  
  ####################
  ## event handlers ##----------
  ####################
  
  mouse_press = function(layer, event) {
    common_mouse_press(layer, event, tdata, meta)
    if ((meta$mode$serie | meta$mode$zoom) & event$button() == Qt$Qt$LeftButton) {
      b$cursor = 18L
      meta$data$xstart = meta$data$xtmp
    }
  }
  
  mouse_move = function(layer, event) {
    if (event$button() != Qt$Qt$NoButton) {
      b$cursor = 0L
    }
    meta$pos = as.numeric(event$pos())
    if (meta$mode$serie) {
      hits = selected(tdata)[meta$data$order]
      meta$data$xtmp[hits] = meta$data$xstart[hits] + meta$pos[1] - meta$start[1]
      qupdate(layer.brush)
      return()
    }
    if (meta$mode$zoom){
      meta$limits[1:2] = meta$limits[1:2] - meta$pos[1] + meta$start[1]
      if (meta$limits[1,1]<extend_ranges(meta$data$xtmp)[1]) {
        meta$limits[1:2] = meta$limits[1:2] - meta$limits[1,1] + extend_ranges(meta$data$xtmp)[1]
      } else if (meta$limits[2,1]>extend_ranges(meta$data$xtmp)[2]) {
        meta$limits[1:2] = meta$limits[1:2] - meta$limits[2,1] + extend_ranges(meta$data$xtmp)[2]
      }
      meta$xat = axis_loc(meta$limits[1:2])
      meta$xlabels = format(meta$xat)
      return()
    }
    rect = as.matrix(qrect(update_brush_size(meta)))
    hits = rectLookup(tree, rect[1, ], rect[2, ])
    if (length(hits)<1) {
      selected(tdata) = FALSE
      return()
    }
    selected(tdata) = meta$data$order[hits]
  }
  
  mouse_release = function(layer, event){
    mouse_move(layer, event)    
    if (meta$mode$serie) {
      meta$limits[1:2] = extend_ranges(range(meta$data$xtmp, na.rm = TRUE))
      tree <<- createTree(data.frame(x=meta$data$xtmp,y=meta$data$ytmp))
    }
  }
  
  mouse_wheel = function(layer, event) {
    pos = as.numeric(event$pos())
    lim = meta$limits
    p = (pos - lim[1, ]) / (lim[2, ] - lim[1, ])
    meta$limits[1:2] = extend_ranges(meta$limits[1:2], -sign(event$delta()) * 0.05 * c(p[1], 1 - p[1]))
    meta$keys = paste("Wheel: Zoom",ifelse(sign(event$delta())>0,"in","out"))
    tmprange = extend_ranges(unlist(meta$data$xtmp))
    meta$limits[1,1] = max(meta$limits[1,1],min(tmprange))
    meta$limits[2,1] = min(meta$limits[2,1],max(tmprange))
    meta$mode$zoom = ifelse(meta$limits[1,1]<=min(tmprange) & meta$limits[2,1]>=max(tmprange), FALSE, TRUE)
    meta$xat = axis_loc(meta$limits[1:2,1])
    meta$xlabels = format(meta$xat)
    qupdate(layer.keys)
  }
  
  identify_hover = function(item, event, ...) {
    if (!b$identify && !meta$mode$serie) return()
    b$cursor = 2L
    meta$pos = as.numeric(event$pos())
    rect = as.matrix(identify_rect(meta))
    if (meta$mode$serie){      
      xdiff = diff(meta$limits[1:2])*0.9/min(sort(table(meta$data$vidgroup),decreasing=TRUE)[1],50)
      ydiff = diff(meta$limits[3:4])*0.9/meta$ngroup$vid
    } else {
      xdiff = meta$radius/layer.root$size$width() * diff(meta$limits[1:2])
      ydiff = meta$radius/layer.root$size$height() * diff(meta$limits[3:4])
    }
    rect = rect + matrix(c(-xdiff,xdiff,-ydiff,ydiff),nrow=2) 
    meta$identified = rectLookup(tree, rect[1, ], rect[2, ])
    
    if (!length(hits <- meta$identified)) return()
    
    if (meta$mode$serie){
      hitgroup = meta$data[hits,c('xtmp','ytmp','vargroup','idgroup','vidgroup'),drop=FALSE]
      if (length(unique(hitgroup$vidgroup))>1){
        hitgroup$dist = sqrt(((hitgroup$xtmp-meta$pos[1])/xdiff)^2+((hitgroup$ytmp-meta$pos[2])/ydiff)^2)
        hitdist = tapply(hitgroup$dist,hitgroup$vidgroup,function(x){sum(1/(x+0.5))})
        hitgroupname = names(hitdist)[which.max(hitdist)]
        hitgroup = hitgroup[hitgroup$vidgroup==hitgroupname,,drop=FALSE]
      }
      hitsall = which(meta$data$vidgroup==hitgroup$vidgroup[1])
      selected(tdata) = hitsall[meta$data$order]   
      qupdate(layer.brush)
      return()
    }
    
    qupdate(layer.identify)
  }
  
  key_press = function(layer, event){
    common_key_press(layer, event, tdata, meta)
    keys = c('M','G','H','U','D','R','Y','Left','Right','Up','Down')
    meta$shiftKey = shift_on(event)
    key = keys[match_key(keys,event)]
    if (!length(key)) return()
    crt_range = diff(range(meta$data$xtmp,na.rm=TRUE))+1
    switch(key, 
           M = switch_serie_mode(meta, tdata),
           G = shift_wrap_gear(meta),
           F = switch_fold_mode(meta, tdata),
           U = separate_group(meta),
           D = mix_group(meta),
           R = switch_area_mode(meta),
           Y = y_wrap_forward(meta,tdata),
           Left = x_wrap_backward(meta,tdata,crt_range),
           Right = x_wrap_forward(meta,tdata,crt_range),
           Up = size_up(meta),
           Down = size_down(meta)
    )
    tree <<- createTree(data.frame(x=meta$data$xtmp,y=meta$data$ytmp))
    qupdate(layer.point)
    qupdate(layer.line)
    qupdate(layer.area)
  }
  
  key_release = function(layer, event){
    common_key_release(layer, event, tdata, meta)
  }
  
  ############
  ## layers ##----------
  ############
  
  point_draw = function(layer,painter){
    col_fill = alpha(tdata$.color[meta$data$order], meta$data$fill*meta$alpha)
    col_stroke = alpha(tdata$.border[meta$data$order], meta$data$stroke*meta$alpha)
    qdrawGlyph(painter, qglyphCircle(r = meta$radius), meta$data$xtmp, meta$data$ytmp,
               fill=col_fill, stroke=col_stroke)
  }
  
  line_draw = function(layer,painter){
    qlineWidth(painter) = meta$radius / 2
    compute_line(meta, tdata)
    qdrawSegment(painter,meta$line$df$xs,meta$line$df$ys,
                 meta$line$df$xe,meta$line$df$ye,stroke=meta$line$df$col)
  }
  
  area_draw = function(layer,painter){
    if (! meta$mode$area) return()
    compute_area(meta, tdata, fun.base)
    qdrawPolygon(painter, meta$area$poly$x, meta$area$poly$y, 
                 stroke=alpha(meta$area$color,0.01), fill=meta$area$color)
  }
  
  brush_draw = function(layer, painter) {
    
    if (any(is.na(meta$pos))) return()
    
    hits = selected(tdata)[meta$data$order]
    if (any(hits)) {compute_area(meta, tdata, fun.base); selected_draw(meta,b,hits,painter)}
    
    if (meta$mode$zoom || meta$mode$serie) return()
    
    draw_brush(layer, painter, tdata, meta)
  }
  
  identify_draw = function(item, painter, exposed, ...) {
    if (!b$identify || !length(hits <- meta$identified)) return()    
    if (meta$ngroup$id==1) {
      info = data.frame(meta$varname$x,meta$data[hits,c('x','yorig','vargroup'),drop=FALSE])
    } else {
      info = data.frame(meta$varname$x, meta$data[hits,c('x','yorig','vargroup','idgroup'),drop=FALSE])
    }
    meta$identify.labels = sprintf('%s: %s\n%s: %s',
      meta$varname$x, paste(info[, 2], collapse = ', '),
      info$vargroup[1], paste(info[, 3], collapse = ', '))
    if (meta$ngroup$id>1) {
      meta$identify.labels = paste(meta$identify.labels,"\nGroup:",paste(info$idgroup,collapse=', '))
    }
    if (length(hits)>1){
      meta$identify.labels = paste("    ",length(hits)," points\n",meta$identify.labels,sep='')
    }
    draw_identify(layer, painter, tdata, meta)
    if (all(tdata$.size==tdata$.size[1])) {
      qdrawGlyph(painter, qglyphCircle(r = sqrt(b$size) * meta$radius),
                 meta$data$xtmp[hits], meta$data$ytmp[hits], stroke = b$color, fill = NA)
    } else {
      qdrawCircle(painter, meta$data$xtmp[hits], meta$data$ytmp[hits],
                  r = sqrt(b$size) * tdata$.size[meta$data$order][hits], 
                  stroke = b$color, fill = NA)
    }
  }
  
  stats_draw = function(layer, painter){
    if (meta$ngroup$id > 1) return()
    if (meta$ngroup$xwrap == 1){
      ytmpacf = unname(tapply(meta$data$ytmp,meta$data$vargroup,function(z) acf(z,lag.max=max(30,max(meta$steplen$xwrap)),plot=FALSE)$acf[meta$steplen$xwrap[1]+1]))
      tmpprint = paste(meta$varname$y,": ACF(lag=",meta$steplen$xwrap[1],"):",round(ytmpacf,2),sep="")
    } else if (meta$ngroup$xwrap==2) {
      ytmpcor = as.vector(by(meta$data[,c('ytmp','xwrapgroup')],meta$data$vargroup,function(x){
        cor(x$ytmp[x$xwrapgroup==1][1:sum(x$xwrapgroup==2)],x$ytmp[x$xwrapgroup==2])
      }))
      tmpprint = paste(meta$varname$y,": Corr. of two series = ",round(ytmpcor,2),sep="")
    } else {
      ytmpR2 = as.vector(by(meta$data[,c('xtmp','ytmp')],meta$data$vargroup,function(x){
        res=summary(lm(x$ytmp~factor(x$xtmp)))$r.squared
      }))
      tmpprint = paste(meta$varname$y,": R square = ",round(ytmpR2,2),sep="")
    }
    if (meta$mode$varUP) {
      qdrawText(painter,tmpprint,
                rep(meta$limits[1,1],meta$ngroup$y),meta$yat-diff(meta$yat[1:2])/2,
                halign='left',valign='bottom',color='gray50')
    } else {
      qdrawText(painter,paste(tmpprint,collapse="\n"),
                meta$limits[1,1],meta$limits[1,2],
                halign='left',valign='bottom',color='gray50')
    }
  }
  
  #####################
  ## draw the canvas ##----------
  #####################
  
  scene = qscene()
  layer.root = qlayer(scene)
  layer.title = qmtext(meta = meta, side = 3)
  layer.xlab = qmtext(meta = meta, side = 1)
  layer.ylab = qmtext(meta = meta, side = 2)
  layer.xaxis = qaxis(meta = meta, side = 1)
  layer.yaxis = qaxis(meta = meta, side = 2)
  layer.grid = qgrid(meta = meta)
  layer.keys = key_layer(meta)
  layer.point = qlayer(paintFun = point_draw,
                       limits = qrect(meta$limits),
                       hoverMoveFun = identify_hover,
                       mousePressFun = mouse_press, 
                       mouseReleaseFun = mouse_release,
                       mouseMoveFun = mouse_move,
                       wheelFun = mouse_wheel,
                       keyPressFun = key_press,
                       keyReleaseFun = key_release,
                       focusInFun = function(layer, event) {
                         common_focus_in(layer, event, data, meta)
                       }, focusOutFun = function(layer, event) {
                         common_focus_out(layer, event, data, meta)
                       }, clip=TRUE)
  layer.line = qlayer(paintFun=line_draw,limits=qrect(meta$limits),clip=TRUE)
  layer.area = qlayer(paintFun=area_draw,limits=qrect(meta$limits),clip=TRUE)
  layer.brush = qlayer(paintFun=brush_draw, limits=qrect(meta$limits))
  layer.identify = qlayer(paintFun=identify_draw, limits=qrect(meta$limits))
  if (series.stats) layer.stats = qlayer(paintFun=stats_draw, limits=qrect(meta$limits))
  
  layer.root[0, 2] = layer.title
  layer.root[2, 2] = layer.xaxis
  layer.root[3, 2] = layer.xlab
  layer.root[1, 1] = layer.yaxis
  layer.root[1, 0] = layer.ylab
  layer.root[1, 2] = layer.grid
  if (series.stats) layer.root[1, 2] = layer.stats
  layer.root[1, 2] = layer.area
  layer.root[1, 2] = layer.line
  layer.root[1, 2] = layer.point
  layer.root[1, 2] = layer.brush
  layer.root[1, 2] = layer.identify
  
  ## set sizes of layers (arrange the layout)
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
  
  ## layout is dynamic (listen to changes in xlab/ylab/xlabels/ylabels...)
  meta$mainChanged$connect(set_layout)
  meta$xlabChanged$connect(set_layout); meta$ylabChanged$connect(set_layout)
  meta$xlabelsChanged$connect(set_layout); meta$ylabelsChanged$connect(set_layout)
  
  ## listeners on the data (which column updates which layer(s))
  d.idx = add_listener(tdata, function(i, j) {
    switch(j, .brushed = qupdate(layer.brush),
           .color = {
             qupdate(layer.point)
             qupdate(layer.line)
             qupdate(layer.area)
           }, {
             qupdate(layer.grid); qupdate(layer.xaxis); qupdate(layer.yaxis)
             layer.point$invalidateIndex()
             layer.line$invalidateIndex()
             layer.area$invalidateIndex()
             qupdate(layer.point)
             qupdate(layer.line)
             qupdate(layer.area)
           })
  })
  qconnect(layer.point, 'destroyed', function(x) {
    remove_listener(tdata, d.idx)
  })
  b$cursorChanged$connect(function() {
    set_cursor(view, b$cursor)
  })
  sync_limits(meta, layer.point, layer.line, layer.area, layer.brush, layer.identify,
              if (series.stats){layer.stats} else {NA})
  meta$manual.brush = function(pos) {
    mouse_move(layer = layer.point, event = list(pos = function() pos))
  }
  
  
  view = qplotView(scene=scene)
  view$setWindowTitle(meta$main)
  attr(view, 'meta') = meta
  view
}



Time.meta2 = setRefClass("Time_meta2", 
                        contains = "CommonMeta",
                        fields = properties(list(
                        varname = 'list', # including the variable names for y, x, idgroup
                        ngroup = 'list', # including the number of groups for y, idgroup
                        data = 'data.frame', # with x,yorig,xtmp,ytmp,vargroup,idgroup,xwrapgroup,finalgroup,order,htvar,htid,htywrap,htfinal,areabaseline
                        mode = 'list', # including area,yfold,xwrap,zoom,serie,idSep,varUP,varDOWN
                        line = 'list', # including df,firstrow,lastrow
                        area = 'list', # including x,y,poly,color
                        hits = 'list',
                        steplen = 'list',
                        singleVarLen = 'integer',
                        shiftKey = 'logical',
                        linkID = 'character',
                        radius = 'numeric',
                        ylab.init = 'character'
                       )))



## Create a new mutaframe for drawing time plots
## 
## The new mutaframe is only used for the qtime function.
## @param data a data frame or mutaframe used for time plot
## @param y a character vertor of all the variable names of interest 
## It is used to link qdata and time_qdata.
## @param link a character vector of the variable names, to link two mutaframes
## @inheritParams qdata
## @return a mutaframe with multiple y's
time_qdata2 = function(regular_qdata, yVar, link) {
  yCol = length(yVar)
  data = as.data.frame(regular_qdata)
  usecol = colnames(data) %in% c(".brushed",".visible",".color",".border",".size")
  setting = data[, usecol]
  settingh = setting[0,]
  data = data[, !usecol]
  newdat = data.frame(.variable="A",.value=0,data[1,],stringsAsFactors=FALSE)[0,]
  for (i in 1:yCol) {
    tmpnewdat = data.frame(.variable=rep(yVar[i],nrow(data)),.value=data[,yVar[i]],data,stringsAsFactors=FALSE)
    tmpnewdat[,yVar[i]] = TRUE
    tmpnewdat[,yVar[-i]] = FALSE
    newdat = rbind(newdat, tmpnewdat)
    settingh = rbind(settingh, setting)
  }
  newdat$.variable = factor(newdat$.variable,levels=yVar)
  rownames(newdat) = 1:nrow(newdat)
  newdat = qdata(newdat,
                 color = if (yCol>1 && all(settingh[,".color"]=='gray15')) {
                   .variable} else {as.character(settingh[,".color"])}, 
                 border = if (yCol>1 && all(settingh[,".border"]=='gray15')) {
                   .variable} else {as.character(settingh[,".border"])}, 
                 size = settingh[,".size"], brushed = settingh[,".brushed"], 
                 visible = settingh[,".visible"])
  link_cat(newdat,link,regular_qdata,link)
  attr(newdat,"regular_nrow") = nrow(data)
  return(newdat)
}



time_meta_initialize2 = function(meta, call, data,
                                 shift, alpha, size, asp,
                                 main, xlab, ylab, ...){
  
  meta$singleVarLen = attr(data,"regular_nrow")
  data = as.data.frame(data)
  
  meta$data = data.frame(x = data[,meta$varname$x],
                         yorig = data$.value)
  
  ## X axis setting
  meta$data$xtmp = meta$data$x
  meta$xlab = ifelse(is.null(xlab), meta$varname$x, xlab)
  
  ## Y axis setting
  meta$varname$y = as.character(unique(data$.variable))
  meta$ngroup$y = length(meta$varname$y)
  meta$data$vargroup = factor(data$.variable,levels=meta$varname$y)
  meta$data$yscaled = meta$data$yorig
  ylist = data[,meta$varname$y,drop=FALSE]
  if (meta$ngroup$y > 1) {
    for (i in 1:meta$ngroup$y) {
      tmprow = ylist[,i]
      tmprowdat = meta$data$yorig[tmprow]
      meta$data$yscaled[tmprow] = (tmprowdat - min(tmprowdat, na.rm = TRUE))/
        diff(range(tmprowdat, na.rm = TRUE))
    }
  }
  meta$data$ytmp = meta$data$yscaled
  meta$ylab = ifelse(is.null(ylab), paste(meta$varname$y,collapse=', '), ylab)
  meta$ylab.init = meta$ylab
  
  ## Group for panel data
  if (is.null(call$group)) {
    meta$varname$g = NULL
    meta$ngroup$id = 1
    meta$data$idgroup = 1    
  } else {
    meta$varname$g = as.character(call$group)
    meta$ngroup$id = length(table(data[,meta$varname$g]))
    meta$data$idgroup = as.factor(data[,meta$varname$g])
  }
  
  ## Other groups
  meta$data$xwrapgroup = 1
  meta$ngroup$xwrap = 1
  meta$data$ywrapgroup = 1
  meta$ngroup$ywrap = 1
  
  ## Group y-axis
  meta$data$htvar=0
  meta$data$htid=0
  meta$data$htywrap=0
  
  ## order the data by vargroup, idgroup, and x
  orderEnter = order(meta$data$vargroup, meta$data$idgroup, meta$data$x, decreasing=FALSE)
  meta$data$order = 1:nrow(meta$data)
  meta$data = meta$data[orderEnter,]
  
  ## Mode settings
  meta$mode$area = FALSE
  meta$mode$yfold = FALSE
  meta$mode$xwrap = FALSE
  meta$mode$ywrap = FALSE
  meta$mode$zoom = FALSE
  meta$mode$serie = FALSE
  meta$mode$idSep = FALSE
  meta$mode$varUP = FALSE
  meta$mode$varDOWN = FALSE
  
  ## Other
  meta$steplen$xwrap = shift
  meta$steplen$ywrap = (0:6)*2
  meta$steplen$id = 0 # vertconst
  meta$steplen$zoom = diff(range(meta$data$xtmp, na.rm = TRUE)) # zoomsize
  meta$shiftKey = FALSE
  meta$linkID = NULL
  
  ## Range, axes, etc.
  meta$limits = matrix(c(extend_ranges(range(meta$data$xtmp, na.rm = TRUE)),
                         extend_ranges(range(meta$data$ytmp, na.rm = TRUE))), nrow=2)
  meta$xat = axis_loc(meta$limits[1:2])
  meta$yat = axis_loc(meta$limits[3:4])
  meta$xlabels = format(meta$xat)
  meta$ylabels = format(meta$yat)

  ## Radius, color, etc.
  meta$radius = size
  meta$alpha = alpha
  meta$data$stroke = 1
  meta$data$fill = 1

  ## Brush etc.
  meta$pos = c(NA, NA)
  meta$start = c(NA, NA)
  meta$brush.move = TRUE
  meta$brush.size = c(diff(meta$limits[1:2]),-diff(meta$limits[3:4]))/30
  
  ## Title
  meta$main = if (is.null(main)) 
    sprintf("Time Plot of %s And %s", meta$varname$x, 
            paste(meta$varname$y, collapse=', ')) else main
  
}

# Update the groups of points to make the line
update_meta_group = function(meta){
  meta$data$finalgroup = paste(meta$data$vargroup, meta$data$idgroup, meta$data$xwrapgroup)
  meta$data$vidgroup = paste(meta$data$vargroup, meta$data$idgroup)
  #meta$data$htfinal = sum(meta$data$htvar, meta$data$htid, meta$data$htywrap)
  meta$ngroup$final = length(unique(meta$data$finalgroup))
  meta$ngroup$vid = length(unique(meta$data$vidgroup))
  meta$ngroup$xwrap = length(unique(meta$data$xwrapgroup))
}

# Update the colors of points when wrapping
update_meta_xwrap_color = function(meta, data){
  color_seq = seq(1,0,length=meta$ngroup$xwrap+1)
  meta$data$fill = color_seq[meta$data$xwrapgroup]
  meta$data$stroke = color_seq[meta$data$xwrapgroup]
}

# Set up meta$line
compute_line = function(meta, data){
  update_meta_group(meta)
  tmpcolor = alpha(data$.color[meta$data$order],meta$data$fill*meta$alpha)
  if (meta$mode$ywrap){
    meta$line$df = data.frame()
    for (i in unique(meta$data$finalgroup)){
      tmpdat = meta$data[meta$data$finalgroup==i,]
      tmpn = nrow(tmpdat)
      for (j in paste('ywrapline',1:(meta$steplen$ywrap[1]),sep='')){
        tmpline = data.frame(xs=tmpdat$xtmp[-tmpn],
                             ys=tmpdat[-tmpn,j],
                             xe=tmpdat$xtmp[-1],
                             ye=tmpdat[-1,j],
                             col=tmpcolor[meta$data$finalgroup==i][-tmpn],
                             id=which(meta$data$finalgroup==i)[-tmpn])
        meta$line$df = rbind(meta$line$df, tmpline[complete.cases(tmpline),])
      }
    }
    return()
  }
  meta$line$lastrow = which(c(diff(as.integer(factor(meta$data$finalgroup)))!=0,TRUE))
  meta$line$firstrow = c(1,(meta$line$lastrow[-length(meta$line$lastrow)]+1))
  meta$line$df = data.frame(xs=meta$data$xtmp[-meta$line$lastrow],
                            ys=meta$data$ytmp[-meta$line$lastrow],
                            xe=meta$data$xtmp[-meta$line$firstrow],
                            ye=meta$data$ytmp[-meta$line$firstrow],
                            col=tmpcolor[-meta$line$lastrow],
                            id=(1:nrow(meta$data))[-meta$line$lastrow])
}

# Set up meta$area
compute_area = function(meta, data, fun.base){
  compute_line(meta, data)
  tmpcolor = alpha(data$.color[meta$data$order],meta$data$fill*meta$alpha/2)
  meta$area$x = data.frame(x1=meta$line$df$xs,x2=meta$line$df$xe,x3=meta$line$df$xe,
                           x4=meta$line$df$xs,x5=meta$line$df$xs,x6=NA)
  if (meta$mode$ywrap){
    areabaseline = meta$data$htvar[meta$line$df$id]
    meta$area$y = data.frame(y1=areabaseline,y2=areabaseline,y3=meta$line$df$ye,
                             y4=meta$line$df$ys,y5=areabaseline,y6=NA)
    meta$area$color = tmpcolor[meta$line$df$id]
  } else {
    areabaseline = tapply(meta$data$ytmp,meta$data$vidgroup,fun.base,na.rm=TRUE)
    meta$data$areabaseline = areabaseline[meta$data$vidgroup]
    meta$area$y = data.frame(y1=meta$data$areabaseline[-meta$line$lastrow],
                             y2=meta$data$areabaseline[-meta$line$lastrow],
                             y3=meta$line$df$ye, y4=meta$line$df$ys,
                             y5=meta$data$areabaseline[-meta$line$lastrow],
                             y6=NA)
    meta$area$color = tmpcolor[-meta$line$lastrow]
  }
  meta$area$poly = data.frame(x=as.vector(as.matrix(t(meta$area$x))),
                              y=as.vector(as.matrix(t(meta$area$y))),
                              group=rep(1:nrow(meta$area$x),each=6))
}

# Draw the selected data in qtime
selected_draw = function(meta,b,hits,painter){
  qdrawGlyph(painter, qglyphCircle(r = meta$radius*2), meta$data$xtmp[hits], 
             meta$data$ytmp[hits], stroke = b$color, fill = b$color)
  qlineWidth(painter) = max(meta$radius,1)
  idx = (hits[-length(hits)] & hits[-1])[-meta$line$lastrow]
  qdrawSegment(painter,meta$area$x[idx,4],meta$area$y[idx,4],
               meta$area$x[idx,3],meta$area$y[idx,3],stroke=b$color)
  if (meta$mode$area){
    tmpx=as.vector(as.matrix(t(meta$area$x[idx,])))
    tmpy=as.vector(as.matrix(t(meta$area$y[idx,])))
    qdrawPolygon(painter, tmpx, tmpy, stroke=alpha(b$color,0.01), fill=alpha(b$color,0.8))
  }
}

# Set limits for yaxis in qtime
meta_yaxis = function(meta) {
  if (meta$mode$varUP) {
    tmpyat = sort(unique(meta$data$htvar))
    meta$yat = tmpyat + diff(tmpyat[1:2])/2
    meta$ylabels = meta$varname$y
    meta$ylab = ""
  } else if (meta$mode$varDOWN) {
    meta$yat = axis_loc(meta$limits[3:4])
    meta$ylabels = format(meta$yat)
    meta$ylab = paste(meta$varname$y,collapse=', ')
    meta$mode$varUP = FALSE
  } else {
    if (meta$ngroup$id==1 | !meta$steplen$id){
      meta$yat = axis_loc(meta$limits[3:4])
    } else {
      meta$yat = (1:meta$ngroup$id-0.5)*meta$steplen$id
    }
    if (meta$steplen$id==0) {
      meta$ylabels = format(meta$yat)
      meta$ylab = meta$ylab.init
    } else {
      meta$ylabels = format(unique(meta$data$idgroup))
      meta$ylab = meta$varname$g
    }
  }
}

# key M for switching the serie mode on the serie mode users can drag any serie horizontally
switch_serie_mode = function(meta,data){
  if (meta$mode$zoom) {
    meta$mode$zoom = FALSE
    meta$mode$serie = FALSE
    return()
  }
  if (meta$ngroup$id>1){
    meta$mode$serie = !meta$mode$serie
    if (!meta$mode$serie) {
      remove_listener(data,meta$linkID)
      meta$linkID = NULL
    } else {
      if (class(data[,meta$varname$g])=='factor'){
        meta$linkID = link_cat(data, meta$varname$g)
      } else {
        message("The group variable is not a factor. Please change to factor before pressing M.")
        meta$mode$serie = FALSE
      }
    }
  } else if (meta$ngroup$y>1) {
    meta$mode$serie = !meta$mode$serie
  }
}

# key G for shifting the wrapping gear, i.e. changing the period/frequency
shift_wrap_gear = function(meta){
  meta$steplen$xwrap = c(meta$steplen$xwrap[-1],meta$steplen$xwrap[1])
}

# key R for turning on/off the area mode
switch_area_mode = function(meta){
  meta$mode$area = !meta$mode$area
}

# key F for fold/unfold the time series by mean
switch_fold_mode = function(meta,data){
  meta$mode$yfold = !meta$mode$yfold
  if (meta$mode$yfold) {
    if (meta$steplen$id == 0) {
      meta$data$ytmp = meta$data$yscaled + meta$data$htvar
    } else {
      meta$data$ytmp =  (meta$data$yscaled-min(meta$data$yscaled,na.rm=TRUE))/diff(range(meta$data$yscaled,na.rm=TRUE)) + meta$data$htid
    }
    hrznbaseline = tapply(meta$data$ytmp,meta$data$vidgroup,mean,na.rm=TRUE)
    meta$data$hrznbaseline = hrznbaseline[meta$data$vidgroup]
    meta$data$hrznydiff = meta$data$ytmp - meta$data$hrznbaseline
    meta$data$ytmp = abs(meta$data$hrznydiff) + meta$data$hrznbaseline
    meta$data$hrzncolor = data$.color[meta$data$order]
    meta$data$hrznborder = data$.border[meta$data$order]
    data$.color[meta$data$order] = c('#E69F00','grey15','#56B4E9')[sign(meta$data$hrznydiff)+2]
    data$.border = data$.color
  } else {
    meta$data$ytmp = meta$data$hrznydiff + meta$data$hrznbaseline
    data$.color[meta$data$order] = meta$data$hrzncolor
    data$.border[meta$data$order] = meta$data$hrznborder
  }
  if (meta$shiftKey | !meta$mode$yfold) {
    meta$limits[3:4] =  extend_ranges(range(meta$data$ytmp,na.rm=TRUE))
    meta_yaxis(meta)
  }
}

# key U for separating the groups by shifting up
separate_group = function(meta){
  if (meta$ngroup$y>1 & meta$shiftKey) {
    meta$data$htvar = as.integer(meta$data$vargroup) - 1
    meta$data$ytmp = meta$data$yscaled + meta$data$htvar
    meta$mode$varUP = TRUE
  } else if (meta$ngroup$id>1) {
    meta$steplen$id = meta$steplen$id + 0.05
    if (meta$steplen$id>1) meta$steplen$id = 1
    meta$data$htid = (as.integer(meta$data$idgroup)-1)*meta$steplen$id
    meta$data$ytmp = (meta$data$yscaled-min(meta$data$yscaled,na.rm=TRUE))/diff(range(meta$data$yscaled,na.rm=TRUE)) + meta$data$htid
  }
  meta$limits[3:4] =  extend_ranges(range(meta$data$ytmp,na.rm=TRUE))
  meta_yaxis(meta)
}

# key D for mixing the groups
mix_group = function(meta){
  meta$mode$varUP = FALSE
  if (meta$ngroup$y>1 & meta$shiftKey) {
    meta$data$htvar = 0
    meta$data$ytmp = meta$data$yscaled
    meta$mode$varDOWN = TRUE
  } else {
    if (meta$ngroup$id>1) {
      meta$steplen$id = meta$steplen$id - 0.05
      if (meta$steplen$id<0) meta$steplen$id = 0
      if (!meta$steplen$id) {
        meta$data$htid = 0
        meta$data$ytmp = meta$data$yscaled
        meta$limits[3:4] =  extend_ranges(range(meta$data$ytmp,na.rm=TRUE))      
      } else {
        meta$data$htid = (as.integer(meta$data$idgroup)-1)*meta$steplen$id
        meta$data$ytmp = (meta$data$yscaled-min(meta$data$yscaled,na.rm=TRUE))/diff(range(meta$data$yscaled,na.rm=TRUE)) + meta$data$htid
      }
    }
  } 
  meta$limits[3:4] =  extend_ranges(range(meta$data$ytmp,na.rm=TRUE))
  meta_yaxis(meta)
}

# key Right for x-wrapping
x_wrap_forward = function(meta,data,crt_range){
  hits = selected(data)[meta$data$order]
  if (meta$mode$serie & sum(hits)) {
    if (min(meta$data$xtmp[hits],na.rm=TRUE)<=max(meta$data$x,na.rm=TRUE)){
      meta$data$xtmp[hits] = meta$data$xtmp[hits] + diff(range(meta$data$x,na.rm=TRUE))/meta$singleVarLen
    }
  } else if (meta$shiftKey) {
    zoombound = max(meta$steplen$xwrap)
    if (zoombound<2) zoombound = diff(range(meta$data$x,na.rm=TRUE))/4
    meta$data$xtmp = meta$data$x %% zoombound
    meta$data$xwrapgroup = ceiling(meta$data$x/zoombound)
    if (sum(meta$data$xtmp==0)){
      meta$data$xwrapgroup[meta$data$xtmp==0] = meta$data$xwrapgroup[which(meta$data$xtmp==0)-1]
      meta$data$xtmp[meta$data$xtmp==0] = zoombound
    }
    meta$limits[1:2] = extend_ranges(meta$data$xtmp)
  } else if (!meta$mode$serie) {
    zoombound = crt_range-meta$steplen$xwrap[1]
    if (meta$steplen$xwrap[1]==1 & zoombound<3){
      zoombound = 3
    } else if (meta$steplen$xwrap[1]!=1 & zoombound<meta$steplen$xwrap[1]){
      zoombound = crt_range %% meta$steplen$xwrap[1]
      if (!zoombound) zoombound = meta$steplen$xwrap[1]
    }
    meta$data$xtmp = meta$data$x %% zoombound
    meta$data$xwrapgroup = ceiling(meta$data$x/zoombound)
    if (sum(meta$data$xtmp==0)){
      meta$data$xwrapgroup[meta$data$xtmp==0] = meta$data$xwrapgroup[which(meta$data$xtmp==0)-1]
      meta$data$xtmp[meta$data$xtmp==0] = zoombound
    }
    meta$limits[1:2] = extend_ranges(meta$data$xtmp)
  }
  update_meta_group(meta)
  update_meta_xwrap_color(meta,data)
  meta$xat = axis_loc(meta$limits[1:2])
  meta$xlabels = format(meta$xat)
}

# key Left for x-backward-wrapping
x_wrap_backward = function(meta,data,crt_range){    
  if (meta$shiftKey) {
    meta$data$xtmp = meta$data$x
    meta$data$xwrapgroup = 1
    meta$steplen$zoom = diff(range(meta$data$xtmp, na.rm = TRUE))
    meta$limits[1:2] = extend_ranges(meta$data$xtmp)
    meta$mode$zoom = FALSE
  } else {
    hits = selected(data)[meta$data$order]
    if (meta$mode$serie & sum(hits)) {
      if (max(meta$data$xtmp[hits],na.rm=TRUE) >= min(meta$data$x,na.rm=TRUE)) {
        meta$data$xtmp[hits] = meta$data$xtmp[hits] - diff(range(meta$data$x,na.rm=TRUE))/meta$singleVarLen
      }
    } else if (!meta$mode$serie) {
      zoombound = crt_range+meta$steplen$xwrap[1]
      if (zoombound>(meta$steplen$zoom+min(meta$data$x,na.rm=TRUE))) {
        zoombound = meta$steplen$zoom+min(meta$data$x,na.rm=TRUE)
      }
      meta$data$xtmp = meta$data$x %% zoombound
      meta$data$xwrapgroup = ceiling(meta$data$x/zoombound)
      if (sum(meta$data$xtmp==0)){
        meta$data$xwrapgroup[meta$data$xtmp==0] = meta$data$xwrapgroup[which(meta$data$xtmp==0)-1]
        meta$data$xtmp[meta$data$xtmp==0] = zoombound
      }
      while (diff(range(meta$data$xtmp,na.rm=TRUE))+1 <= crt_range &
               zoombound<meta$steplen$zoom+min(meta$data$x,na.rm=TRUE)) {
        zoombound = zoombound+max(meta$steplen$xwrap)
        if (zoombound>(meta$steplen$zoom+min(meta$data$x,na.rm=TRUE))) {
          zoombound = meta$steplen$zoom+min(meta$data$x,na.rm=TRUE)
        }
        meta$data$xtmp = meta$data$x %% zoombound
        meta$data$xwrapgroup = ceiling(meta$data$x/zoombound)
        if (sum(meta$data$xtmp==0)){
          meta$data$xwrapgroup[meta$data$xtmp==0] = meta$data$xwrapgroup[which(meta$data$xtmp==0)-1]
          meta$data$xtmp[meta$data$xtmp==0] = zoombound
        }
      }
      meta$limits[1:2] = extend_ranges(meta$data$xtmp)
    }
  }
  update_meta_group(meta)
  update_meta_xwrap_color(meta,data)
  meta$xat = axis_loc(meta$limits[1:2])
  meta$xlabels = format(meta$xat)
}

# key Y for y-wrapping, and Shift+Y for y-backward-wrapping
y_wrap_forward = function(meta,data){
  l = length(meta$steplen$ywrap)
  meta$steplen$ywrap = if (meta$shiftKey){meta$steplen$ywrap[c(l,1:(l-1))]} else {meta$steplen$ywrap[c(2:l,1)]}
  if (meta$steplen$ywrap[1] == 0){
    meta$data$htvar = if (meta$mode$varUP){as.integer(meta$data$vargroup)-1} else 0
    meta$mode$ywrap = FALSE
    meta$data$ywrapgroup = 1
    meta$data$ytmp = if (meta$mode$yfold){
      meta$data$hrznbaseline + abs(meta$data$hrznydiff)
    } else if (meta$steplen$id == 0){
      meta$data$yscaled + meta$data$htvar
    } else {
      (meta$data$yscaled-min(meta$data$yscaled,na.rm=TRUE))/diff(range(meta$data$yscaled,na.rm=TRUE)) + meta$data$htid
    }
    meta$limits[3:4] = extend_ranges(range(meta$data$ytmp,na.rm=TRUE))
    meta_yaxis(meta)
    return()
  }
  meta$mode$area = TRUE
  meta$mode$ywrap = TRUE
  if (meta$mode$yfold){
    cutbound = tapply(meta$data$hrznbaseline + abs(meta$data$hrznydiff),
                      meta$data$vargroup,function(x){
                        seq(min(x),max(x),length=meta$steplen$ywrap[1]/2+1)
                        })
  }else{
    cutbound = tapply(meta$data$yscaled,meta$data$vargroup,function(x){
      seq(min(x),max(x),length=meta$steplen$ywrap[1]+1)
    })
  }
  meta$data$htvar = (as.integer(meta$data$vargroup)-1) * diff(cutbound[[1]][1:2])*1.05
  cutbound2 = lapply(cutbound,function(x){
    x[1]=x[1]-1
    x[meta$steplen$ywrap[1]+1]=x[meta$steplen$ywrap[1]+1]+1
    return(x)})
  meta$data$ywrapgroup = 1
  meta$data[,paste('ywrapline',1:(meta$steplen$ywrap[1]),sep='')] = NA
  for (i in 1:meta$ngroup$y){
    tmprows = (meta$data$vargroup==meta$varname$y[i])
    meta$data$ywrapgroup[tmprows] = as.integer(cut(meta$data$yscaled[tmprows],cutbound2[[i]]))
    tmpbound = cutbound[[i]]
    if (meta$mode$yfold){      
      tmpbound[1:(meta$steplen$ywrap[1]/2)] = tmpbound[1:(meta$steplen$ywrap[1]/2)+1]
      meta$data$ytmp[tmprows] = meta$data$yscaled[tmprows] - tmpbound[meta$data$ywrapgroup[tmprows]]
      tmpfold = (meta$data$ywrapgroup[tmprows] > meta$steplen$ywrap[1]/2)
      meta$data$ytmp[tmprows] = meta$data$ytmp[tmprows] * c(-1,1)[tmpfold+1] + meta$data$htvar[tmprows]
    } else {
      meta$data$ytmp[tmprows] = meta$data$yscaled[tmprows] + meta$data$htvar[tmprows] - tmpbound[meta$data$ywrapgroup[tmprows]]
    }
    for (j in 1:(meta$steplen$ywrap[1])) {
      tmpline = which(meta$data$ywrapgroup==j & tmprows)
      meta$data[tmpline,paste('ywrapline',j,sep='')] = meta$data$ytmp[tmpline]
      tmpboundary = setdiff(which(tmprows),tmpline)
      tmpupper = tmpboundary[meta$data$ywrapgroup[tmpboundary]>j]
      tmplower = tmpboundary[meta$data$ywrapgroup[tmpboundary]<j]
      if (meta$mode$yfold & j<=(meta$steplen$ywrap[1]/2) ) {
        tmp = tmpupper
        tmpupper = tmplower
        tmplower = tmp
      }
      meta$data[tmpupper,paste('ywrapline',j,sep='')] = diff(tmpbound[1:2]) + meta$data$htvar[tmpline][1]
      meta$data[tmplower,paste('ywrapline',j,sep='')] = meta$data$htvar[tmpline][1]
    }
  }
  update_meta_group(meta)
  update_meta_xwrap_color(meta,data)
  if (meta$ngroup$y > 1) {
    meta$limits[3:4] = extend_ranges(c(0,meta$ngroup$y*diff(cutbound[[1]][1:2])))
  } else {
    meta$limits[3:4] = extend_ranges(range(meta$data$ytmp,na.rm=TRUE))
  }
  meta_yaxis(meta)
}

# key Up/Down for adjusting the point size / line width
size_up = function(meta){
  meta$radius = meta$radius + 1
}
size_down = function(meta){
  meta$radius = max(0.1, meta$radius - 1)
}
