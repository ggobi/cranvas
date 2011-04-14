qtime <- function(time,y,data){
  tmpdata <- mutaframe(time=data[,time],zoomgroup=rep(1,nrow(data)))
  cy <- y[1]

  brush_mouse_press <- function(layer, event) {
    .bstart <<- as.numeric(event$pos())
    if (event$button() == Qt$Qt$RightButton) {
      .bmove <<- FALSE
    }
    if (event$button() == Qt$Qt$LeftButton) {
      .bmove <<- TRUE
    }
    if (fisheye_toggle) {
      tmp_fisheye_bars <<- sort(fisheye_bars, decreasing=FALSE)
      fisheye_bars <<- .bstart[c(1,1)]+c(0,0.5)
      qupdate(main_layer)
    }
  }

  brush_mouse_release <- function(layer, event){
    .bend <- as.numeric(event$pos())
    if (!fisheye_toggle){
      idx <- tmpdata$time>min(.bstart[1],.bend[1]) &
      data[,cy]>min(.bstart[2],.bend[2]) &
      tmpdata$time<max(.bstart[1],.bend[1]) &
      data[,cy]<max(.bstart[2],.bend[2])
      selected(data) <- idx
      if (length(idx)) qupdate(brush_layer)
    } else {
      if (.bend[1]<=.bstart[1]){
        fisheye_bars[1] <<- .bend[1]
        fisheye_bars[2] <<- tmp_fisheye_bars[2]
        #fisheye_bars <<- sort(fisheye_bars, decreasing=FALSE)
        if (.bstart[1]>=tmp_fisheye_bars[2]){
          left <- which(tmpdata$time<.bstart[1])
          right <- which(tmpdata$time>=.bstart[1])
          center <- NULL
          tmpdata$time[left] <- (tmpdata$time[left]-min(tmpdata$time[left]))/(max(tmpdata$time[left])-min(tmpdata$time[left]))*(.bend[1]-min(tmpdata$time[left]))+min(tmpdata$time[left])
          tmpdata$time[right] <- (tmpdata$time[right]-min(tmpdata$time[right]))/(max(tmpdata$time[right])-min(tmpdata$time[right]))*(max(tmpdata$time[right])-.bend[1])+min(tmpdata$time[right])
        } else {
          left <- which(tmpdata$time<.bstart[1])
          right <- which(tmpdata$time>fisheye_bars[2])
          center <- which(tmpdata$time>=.bstart[1] &
                          tmpdata$time<=fisheye_bars[2])
          if (length(center)==0) {
            center <- NULL
            tmpdata$time[left] <- (tmpdata$time[left]-min(tmpdata$time[left]))/(max(tmpdata$time[left])-min(tmpdata$time[left]))*(.bend[1]-min(tmpdata$time[left]))+min(tmpdata$time[left])
          tmpdata$time[right] <- (tmpdata$time[right]-min(tmpdata$time[right]))/(max(tmpdata$time[right])-min(tmpdata$time[right]))*(max(tmpdata$time[right])-.bend[1])+min(tmpdata$time[right])
          } else {
            tmpdata$time[left] <- (tmpdata$time[left]-min(tmpdata$time[left]))/(max(tmpdata$time[left])-min(tmpdata$time[left]))*(fisheye_bars[1]-min(tmpdata$time[left]))+min(tmpdata$time[left])
            tmpdata$time[right] <- (tmpdata$time[right]-min(tmpdata$time[right]))/(max(tmpdata$time[right])-min(tmpdata$time[right]))*(max(tmpdata$time[right])-fisheye_bars[2])+min(tmpdata$time[right])
            tmpdata$time[center] <- (tmpdata$time[center]-min(tmpdata$time[center]))/(max(tmpdata$time[center])-min(tmpdata$time[center]))*(fisheye_bars[1]-fisheye_bars[2])+min(tmpdata$time[center])
          }
        }
      } else {
        fisheye_bars[2] <<- .bend[1]
        fisheye_bars[1] <<- tmp_fisheye_bars[1]
        fisheye_bars <<- sort(fisheye_bars, decreasing=FALSE)
        if (.bstart[1]<=tmp_fisheye_bars[1]){
          left <- which(tmpdata$time<=.bstart[1])
          right <- which(tmpdata$time>.bstart[1])
          center <- NULL
          tmpdata$time[left] <- (tmpdata$time[left]-min(tmpdata$time[left]))/(max(tmpdata$time[left])-min(tmpdata$time[left]))*(.bend[1]-min(tmpdata$time[left]))+min(tmpdata$time[left])
          tmpdata$time[right] <- (tmpdata$time[right]-min(tmpdata$time[right]))/(max(tmpdata$time[right])-min(tmpdata$time[right]))*(max(tmpdata$time[right])-.bend[1])+min(tmpdata$time[right])
        } else {
          left <- which(tmpdata$time<fisheye_bars[1])
          right <- which(tmpdata$time>.bstart[1])
          center <- which(tmpdata$time>=fisheye_bars[1] &
                          tmpdata$time<=.bstart[1])
          if (length(center)==0) {
            center <- NULL
            tmpdata$time[left] <- (tmpdata$time[left]-min(tmpdata$time[left]))/(max(tmpdata$time[left])-min(tmpdata$time[left]))*(.bend[1]-min(tmpdata$time[left]))+min(tmpdata$time[left])
          tmpdata$time[right] <- (tmpdata$time[right]-min(tmpdata$time[right]))/(max(tmpdata$time[right])-min(tmpdata$time[right]))*(max(tmpdata$time[right])-.bend[1])+min(tmpdata$time[right])
          } else {
            tmpdata$time[left] <- (tmpdata$time[left]-min(tmpdata$time[left]))/(max(tmpdata$time[left])-min(tmpdata$time[left]))*(fisheye_bars[1]-min(tmpdata$time[left]))+min(tmpdata$time[left])
            tmpdata$time[right] <- (tmpdata$time[right]-min(tmpdata$time[right]))/(max(tmpdata$time[right])-min(tmpdata$time[right]))*(max(tmpdata$time[right])-fisheye_bars[2])+min(tmpdata$time[right])
            tmpdata$time[center] <- (tmpdata$time[center]-min(tmpdata$time[center]))/(max(tmpdata$time[center])-min(tmpdata$time[center]))*(fisheye_bars[1]-fisheye_bars[2])+min(tmpdata$time[center])
          }
        }
      }
      main_layer$setLimits(qrect(range(tmpdata$time),
                       range(data[,cy])))
      brush_layer$setLimits(qrect(range(tmpdata$time),
                       range(data[,cy])))
      qupdate(main_layer)
      qupdate(xaxis)
    }
  }

  mouse_double_click <- function(layer, event){
    fisheye_toggle <<-!fisheye_toggle
    tmpdata$time <- data[,time]
    tmpdata$zoomgroup <- rep(1,nrow(data))
    if (fisheye_toggle){
      fisheye_bars <<- c(0.3,0.7)*(max(tmpdata$time)-min(tmpdata$time))+min(tmpdata$time)
    }
    main_layer$setLimits(qrect(range(tmpdata$time),
                       range(data[,cy])))
    brush_layer$setLimits(qrect(range(tmpdata$time),
                       range(data[,cy])))
    qupdate(main_layer)
    qupdate(xaxis)
  }

  mouse_wheel <- function(layer, event){
   # tmp<<-event
   # print(event$type())
  }

  key_press <- function(layer, event){
    crt_range <- max(tmpdata$time)-min(tmpdata$time)+1
    if (event$key()==Qt$Qt$Key_Plus){
      zoombound=min(round(0.96*crt_range),crt_range-1)
      if (zoombound<3)zoombound <- 3
      tmpdata$time <- data[,time]%%zoombound
      tmpdata$zoomgroup <- ceiling(data[,time]/zoombound)
      if (sum(tmpdata$time==0)){
        tmpdata$zoomgroup[tmpdata$time==0] <- tmpdata$zoomgroup[which(tmpdata$time==0)-1]
        tmpdata$time[tmpdata$time==0] <- zoombound
      }
    }
    if (event$key()==Qt$Qt$Key_Minus){
      zoombound=max(round(crt_range*25/24),crt_range+1)
      if (zoombound>(max(data[,time])-min(data[,time])+1)) zoombound <- max(data[,time])-min(data[,time])+1
      tmpdata$time <- data[,time]%%zoombound
      tmpdata$zoomgroup <- ceiling(data[,time]/zoombound)
      if (sum(tmpdata$time==0)){
        tmpdata$zoomgroup[tmpdata$time==0] <- tmpdata$zoomgroup[which(tmpdata$time==0)-1]
        tmpdata$time[tmpdata$time==0] <- zoombound
      }
    }
    main_layer$setLimits(qrect(range(tmpdata$time),
                       range(data[,cy])))
    brush_layer$setLimits(qrect(range(tmpdata$time),
                       range(data[,cy])))
    qupdate(main_layer)
    qupdate(xaxis)
  }

  main_draw <- function(layer,painter){
    if (fisheye_toggle){
      qdrawRect(painter,fisheye_bars[1]-0.25,min(data[,cy]),
                fisheye_bars[1]+0.25,max(data[,cy]),
                stroke='grey',fill='grey')
      qdrawRect(painter,fisheye_bars[2]-0.25,min(data[,cy]),
                fisheye_bars[2]+0.25,max(data[,cy]),
                stroke='grey',fill='grey')
    }
    color=gray(seq(0,0.8,length=max(tmpdata$zoomgroup)))
    for (i in 1:max(tmpdata$zoomgroup)) {
      qdrawCircle(painter,tmpdata[tmpdata$zoomgroup==i,1],
                  data[tmpdata$zoomgroup==i,cy],2,
                  fill=color[i],stroke=color[i])
      qdrawLine(painter,tmpdata[tmpdata$zoomgroup==i,1],
                data[tmpdata$zoomgroup==i,cy],stroke=color[i])
    }
  }
  brush_draw <- function(layer, painter) {
    idx <- selected(data)
    if (any(idx)){
      txt <- paste("Point ",rownames(data)[idx],"\nTime: ",
                   data[idx,time],"\nValue: ",data[idx,cy],
                   sep="")
      qdrawText(painter,txt,tmpdata[idx,1],data[idx,cy])
    }

  }
  scene <- qscene()
  root_layer <- qlayer(scene)
  xaxis <- qaxis(root_layer, data = tmpdata$time, side = 1,
                 limits=range(tmpdata$time),
                 row = 2, col = 1)
  yaxis <- qaxis(root_layer, data = data[,cy], side = 2,
                 limits=range(data[,cy]),
                 row = 1, col = 0)
  fisheye_toggle <<- FALSE

  main_layer <- qlayer(root_layer,paintFun=main_draw,
                       mousePressFun=brush_mouse_press,
                       mouseReleaseFun=brush_mouse_release,
                       mouseDoubleClickFun=mouse_double_click,
                       wheelFun=mouse_wheel,
                       keyPressFun=key_press,
                       limits=qrect(range(tmpdata$time),
                       range(data[,cy])), row = 1, col = 1)
  brush_layer <- qlayer(root_layer, brush_draw,
                        limits=qrect(range(tmpdata$time),
                        range(data[,cy])), row = 1, col = 1)

    layout = root_layer$gridLayout()
    layout$setRowPreferredHeight(0, 30)
    layout$setColumnPreferredWidth(0, 30)
    layout$setRowPreferredHeight(2, 30)
    layout$setColumnMaximumWidth(2, 10)
    layout$setRowStretchFactor(0, 0)
    layout$setColumnStretchFactor(0, 0)
    layout$setRowStretchFactor(2, 0)

  view <- qplotView(scene=scene)
  view

}
