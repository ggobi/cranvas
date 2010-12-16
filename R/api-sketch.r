#qtbase v 0.8-4
#qtpaint v 0.7.9
#svn 425

#####################################################################
###In-progress Example###
##may not run

###End in progress###
####################################################################
######################################################################
###Example###

##not run!
#library(plumbr)
#source(".../api_0.1-2.R")

##data
#n<-10000
#x<-rnorm(n,50,25)
#y<-rnorm(n,50,25)
#df1<-mutaframe(X=x,Y=y)
#width<-400
#height<-400
#rectW<-2
#rectH<-2

#axes <- function(item, painter) {
#	qfont(painter) <- qfont(pointsize=12)
#	pos <- as.matrix(item$geometry) + 5
#	qdrawText(painter, colnames(df1)[1], pos[2], pos[4], "right", "bottom")
#	qdrawText(painter, colnames(df1)[2], pos[1], pos[3], "left", "top")
#}
#
#plot1<-new_plot(width,height,xrange=range(df1[,1]),yrange=range(df1[,2]))
#
#mark<- glyph(left=df1[,1]-min(df1[,1]),bottom=df1[,2]-min(df1[,2]),stroke=NA,fill='black',size=5,parent=plot1)
#mark2<-rect(left=df1[,1]-(0.5*rectW)-min(df1[,1]),bottom=df1[,2]- (0.5*rectH)- min(df1[,2]),height=rectH, width=rectW,stroke=NA,fill=col2rgb(rgb(1,seq(0,1,length=nrow(df1)),0,0.5),T),parent=plot1)
#mark3<-line(left=50+c(1:90)*sin(6*pi*c(1:90)/100)-min(df1[,1]),bottom=50+c(1:90)*cos(6*pi*c(1:90)/100)-min(df1[,2]),stroke="blue",width=3,parent=plot1)
#mark4<-hbar(bottom=50-min(df1[,2]),left=c(-9,59)-min(df1[,1]),right=c(29,96)-min(df1[,2]),parent=plot1)
#mark5<-vbar(left=50-min(df1[,1]),bottom=c(-33,83)-min(df1[,2]),top=c(0,115)-min(df1[,2]),parent=plot1, stroke="blue")
#mark6<-text(bottom=50-min(df1[,2]),left=50-min(df1[,1]),text="test",parent=plot1)
#
#add_layer(parent=plot1,mark=mark)
#add_layer(parent=plot1,mark=mark2)
#add_layer(parent=plot1,mark=mark3)
#add_layer(parent=plot1,mark=mark4)
#add_layer(parent=plot1,mark=mark5)
#add_layer(parent=plot1,mark=mark6)

#view <- qplotView(scene = plot1$scene)

#overlay<-view$overlay()
#axesOverlay<-qlayer(overlay,axes)
#print(view)

#modify_layer(layerID=1,parent=plot1,show=F)
#modify_layer(layerID=1,parent=plot1,show=T)
#modify_layer(layerID=3,parent=plot1,alpha=0.3)
#modify_layer(layerID=6,parent=plot1,shift_right=200)


###End Example###
######################################################################
######################################################################
###Mark Constructors###
# inspired from protovis
# arguments are in data units unless otherwise specified

#Reference from protovis
# protovis object: dot
# reference: http://vis.stanford.edu/protovis/docs/dot.html
# minimum properties: left & bottom
# top - the distance from the top edge of the parent panel to the dot center.
# left - the distance from the left edge of the parent panel to the dot center.
# bottom - the distance from the bottom edge of the parent panel to the dot center.
# right - the distance from the right edge of the parent panel to the dot center.
# size - the size (proportional to area) of the dot. NOTE: see changes below

#Changes/Additions from protovis
# xrange - (xmin,xmax) of parent panel
# yrange - (ymin,ymax) of parent panel
# size - radius
# stroke - color of glyph outline
# fill - color to fill glyph

glyph <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, fill = "black", stroke = NA, size= 5,parent=NULL) {
  #error handling
  #need one each of left/right; top/bottom
  if( (is.null(left) & is.null(right)) |
      (is.null(top) & is.null(bottom)) |
      (!is.null(left) & !is.null(right)) |
      (!is.null(top) & !is.null(bottom))) {
    print("incorrect arguments: need one each of left/right and top/bottom")
  }
  structure(list(left = left,right=right,top=top, bottom = bottom,fill = fill, stroke = stroke,size=size,parent=parent), class = c("cranvas", "glyph"))
}

#Reference from protovis:
# protovis object - bar
# reference - http://vis.stanford.edu/protovis/docs/bar.html
# minimum properties: left,bottom, width, height
# top - the distance from the top edge of the parent panel.
# left - the distance from the left edge of the parent panel.
# bottom - the distance from the bottom edge of the parent panel.
# right - the distance from the right edge of the parent panel.
# width - the width of the bar.
# height - the height of the bar

#Changes/Additions from protovis:
# xrange - (xmin,xmax) of parent panel
# yrange - (ymin,ymax) of parent panel
# fill - color to fill rect
# stroke - color of rect outline

#Comments:
## Do we really need to modify arguments to support protovis naming conventions only to modify them again to support qtpaint property conventions?
##   Wouldn't it be easier to have qtpaint support property conventions? Note this question does not address superficial naming conventions, rather
##   observing that protovis defines the shape using width and height properties. qdrawRect explicitly uses left right top bottom.

rect <- function(left = NULL,right=NULL,top=NULL, bottom = NULL, width=NULL, height=NULL,fill = "black", stroke = NULL,parent=NULL) {
  if( (is.null(left) & is.null(right)) |
      (is.null(right) & is.null(width)) |
      (is.null(left) & is.null(width)) |
      (!is.null(left) & !is.null(right) & !is.null(width))|
      (is.null(top) & is.null(bottom)) |
      (is.null(top) & is.null(height)) |
      (is.null(bottom) & is.null(height)) |
      (!is.null(top) & !is.null(bottom) & !is.null(height))) {
    print("need 2 of 3 in *left,right,width* and *top,bottom,height*")
  }
  structure(list(left = left, bottom = bottom, right = right, top = top, height =height, width = width, parent = parent, fill = fill, stroke = stroke), class = c("cranvas", "rect"))
}

#Reference from protovis:
# protovis object - line
# reference - http://vis.stanford.edu/protovis/docs/line.html
# minimum properties: left, bottom
# top - the distance from the top edge of the parent panel to the line center.NOTE: see changes below
# left - the distance from the left edge of the parent panel to the line center.NOTE: see changes below
# bottom - the distance from the bottom edge of the parent panel to the line center.NOTE: see changes below
# right - the distance from the right edge of the parent panel to the line center.NOTE: see changes below
# defining the top property instead of bottom, the line is flipped vertically:
# using right instead of left flips horizontally

#Changes/Additions from protovis:
# top,left,bottom,right measure distance to segment endpoints #like specs for provis rule (see below)

line <- function(left = NULL,right=NULL,top=NULL, bottom = NULL,stroke = "black", width=1,parent=NULL) {
  if( ( !is.null(left) & !is.null(right) ) |
      ( is.null(left) & is.null(right) )   |
      ( !is.null(top) & !is.null(bottom) ) |
      ( is.null(top) & is.null(bottom) ) )  {
    print("need one of {top,bottom} and {left,right}")
  }
  structure(list(left = left,right=right, top=top,bottom = bottom, stroke = stroke, width=width,parent=parent), class = c("cranvas", "line"))
}

#Reference from protovis:
# protovis object - rule
# reference -  http://vis.stanford.edu/protovis/docs/rule.html
# minimum properties:
#  horizontal, entire width -bottom
#  horizontal, specified width - bottom, left right
#  vertical, full height - left
#  vertical, specified height - left, top, bottom

hbar <- function(width = 1,top=NULL, bottom = NULL, left = NULL, right=NULL,stroke = 'black',parent=parent) {
  if( !is.null(top) & !is.null(bottom) ) {
    print( "need only one of {top,bottom}" )
  }
  if ( ( !is.null(left) & is.null(right) ) |
       ( is.null(left) & !is.null(left) ) ) {
    print( "need both left and right")
  }
  structure( list( left=left, right=right, top=top, bottom=bottom, stroke=stroke, width=width, parent=parent), class = c("cranvas", "hbar"))
}

vbar <- function(left = NULL, right = NULL, top = NULL, bottom = NULL, stroke = 'black', width=1, parent=NULL) {
  if( !is.null(left) & !is.null(right) ) {
    print( "need only one of {left,right}" )
  }
  if ( ( !is.null(top) & is.null(bottom) ) |
       ( is.null(top) & !is.null(bottom) ) ) {
    print( "need both top and bottom" )
  }
  structure( list( left=left, right=right, top=top, bottom=bottom, stroke=stroke, width=width, parent=parent), class = c("cranvas", "vbar"))
}

#Reference from protovis:
# protovis object - labels
# reference - http://vis.stanford.edu/protovis/docs/label.html
# minimum properties: left, bottom, text
# top - the distance from the top edge of the parent panel to the text anchor.
# left - the distance from the left edge of the parent panel to the text anchor.
# bottom - the distance from the bottom edge of the parent panel to the text anchor.
# right - the distance from the right edge of the parent panel to the text anchor.
# textAlign - horizontal alignment.
# textBaseline - vertical alignment.
# textMargin - margin to offset from the text anchor.
# textAngle - rotation angle, in radians.

text <- function(top = NULL, left = NULL, bottom = NULL, right = NULL, text = NULL, stroke = "black", valign = "center", halign = "center", rot = 0, margin = NULL, font = "Arial", italic = F, pointsize = 12, parent = NULL){
  if( ( is.null(left) & is.null(right) )   |
      ( !is.null(left) & !is.null(right) ) |
      ( is.null(top) & is.null(bottom) )   |
      ( !is.null(top) & !is.null(bottom) ) ) {
  print( "need one of {left,right} and {top,bottom}" )
  }

  structure( list( top = top, left = left, bottom = bottom, right = right, text = text, stroke = stroke, valign = valign, halign = halign, rot = rot, margin = margin, font = font, italic = italic, pointsize = pointsize, parent = parent), class = c("cranvas", "text") )
}

###End Mark Constructors###
######################################################################
#####################################################################
###Draw Wrappers###
# Thin wrappers around qtpaint drawing functions that basically translate
# argument names.  (And maybe wrap around any qtpaints that need to be
# temporarily worked around).

draw <- function(mark, canvas) UseMethod("draw")

draw.glyph <- function(mark, canvas) {
  if (is.null(mark$left) & !is.null(mark$right)) {
    x <- mark$parent$limits$width() - mark$right
  } else {
    x <- mark$left
  }

  if (is.null(mark$bottom) & !is.null(mark$top)){
    y <- mark$parent$limits$height() - mark$top
  } else {
    y <- mark$bottom
  }

  x<-x + mark$parent$limits$left()
  y<-y + mark$parent$limits$top()
  circle <- qglyphCircle(r=mark$size)
  qdrawGlyph(canvas, circle, x=x, y=y,stroke = mark$stroke, fill = mark$fill)
}

draw.rect <- function(mark, canvas) {
  if(!is.null(mark$left) & !is.null(mark$width)) {
    xleft <- mark$left
    xright <- mark$left + mark$width
  } else if (!is.null(mark$right) & !is.null(mark$width)) {
    xleft <- mark$parent$limits$width() - mark$right - mark$width
    xright <- xleft + mark$width
  } else {
    xleft <- mark$left
    xright <- mark$left + ( mark$parent$limits$width() - mark$left - mark$right )
  }

  if(!is.null(mark$bottom) & !is.null(mark$height)) {
    ybottom <- mark$bottom
    ytop <- mark$bottom + mark$height
  } else if (!is.null(mark$top) & !is.null(mark$height)) {
    ybottom <-mark$parent$limits$height() - mark$top - mark$height
    ytop<- ybottom + mark$height
  } else {
    ybottom <- mark$bottom
    ytop<- mark$bottom + ( mark$parent$limits$height() - mark$bottom - mark$top )
  }

  xleft <- xleft + mark$parent$limits$left()
  xright <- xright + mark$parent$limits$left()
  ytop <- ytop + mark$parent$limits$top()
  ybottom <- ybottom + mark$parent$limits$top()
  qdrawRect(p=canvas, xleft=xleft, ybottom, xright, ytop,stroke = mark$stroke, fill = mark$fill)
}

draw.line <- function(mark, canvas) {
  if( !is.null(mark$left) ){
    x <- mark$left
  } else {
    x <- sum(range(mark$right)) - mark$right
  }

  if( !is.null(mark$bottom) ){
    y <- mark$bottom
  } else {
    y <- sum(range(mark$top)) - mark$top
  }

  x <- x + mark$parent$limits$left()
  y <- y + mark$parent$limits$top()
  qlineWidth(canvas) <- mark$width
  qdrawLine(canvas, x, y, stroke = mark$stroke)
}

draw.hbar <- function(mark,canvas) {
  if( is.null(mark$left) ) {
    temp <- vector (mode = "numeric", length = max(length(mark$top), length(mark$bottom)) * 3)
    for (i in 1:(length(temp)/3) ) {
      temp[3 * i - 2] <- mark$parent$limits$left()
      temp[3 * i - 1] <- mark$parent$limits$right()
      temp[3 * i] <- NA
    }
    x <- temp

  } else {
    temp <- vector( mode="numeric", length=length(mark$left) * 3 )
    for ( i in 1:length(mark$left) ) {
      temp[3 * i - 2] <- mark$left[i]
      temp[3 * i - 1] <- mark$parent$limits$width() - mark$right[i]
      temp[3 * i] <- NA
    }
    x <- temp
    x <- x + mark$parent$limits$left()
  }

  if( !is.null(mark$bottom) ) {
    temp <- vector( mode = "numeric", length = length(mark$bottom) * 3 )
    for ( i in 1:length(mark$bottom)){
      temp[3 * i - 2] <- mark$bottom[i]
      temp[3 * i - 1] <- mark$bottom[i]
      temp[3 * i] <-NA
    }
    y <- temp + mark$parent$limits$top()
  } else {
    temp <- vector( mode = "numeric", length = length(mark$top) * 3 )
    for ( i in 1:length(mark$top)){
      temp[3 * i - 2] <-  mark$parent$limits$bottom() - mark$top[i]
      temp[3 * i - 1] <- mark$parent$limits$bottom() - mark$top[i]
      temp[3 * i] <-NA
    }
    y <- temp
  }

  qlineWidth(canvas) <- mark$width
  qdrawLine(canvas, x, y, stroke= mark$stroke)
}

draw.vbar <- function(mark, canvas) {
  if( is.null(mark$top) ) {
    temp <- vector (mode = "numeric", length = max(length(mark$left), length (mark$right)) * 3)
    for (i in 1:(length(temp) / 3)) {
      temp[3 * i - 2] <- mark$parent$limits$top()
      temp[3 * i - 1] <- mark$parent$limits$bottom()
      temp[3 * i] <- NA
    }
    y <- temp

#    y <- c(mark$parent$limits$top(), mark$parent$limits$bottom())
  } else {
    temp <- vector( mode = "numeric", length = length(mark$top) * 3 )
    for ( i in 1:length(mark$top) ) {
      temp[3 * i - 2] <- mark$bottom[i]
      temp[3 * i - 1] <- mark$parent$limits$bottom() - mark$top[i]
      temp[3 * i] <- NA
    }
    y<-temp
    y<- y + mark$parent$limits$top()
  }

  if( !is.null(mark$left) ) {
    temp <- vector( mode = "numeric", length = length(mark$left) * 3)
    for (i in 1:length(mark$left)) {
      temp[3 * i - 2] <- mark$left[i]
      temp[3 * i - 1] <- mark$left[i]
      temp[3 * i] <- NA
    }
    x <- temp
   # x <- c( mark$left, mark$left)
    x <- x + mark$parent$limits$left()
  } else {
    temp <- vector( mode = "numeric", length = length(mark$right) * 3)
    for (i in 1:length(mark$right)) {
      temp[3 * i - 2] <- mark$parent$limits$height() - mark$right[i]
      temp[3 * i - 1] <- mark$parent$limits$height() - mark$right[i]
      temp[3 * i] <- NA
    }
    x <- temp
  #  x <- c( mark$parent$limits$height() - mark$right, mark$parent$limits$height() - mark$right)
    x <- x + mark$parent$limits$left()
  }

  qlineWidth(canvas) <- mark$width
  qdrawLine( canvas, x, y, stroke = mark$stroke )
}

draw.text<-function(mark,canvas){
  if( !is.null(mark$bottom) ) {
    anchor_bottom <- mark$bottom
  } else {
    anchor_bottom <- mark$parent$limits$height() - mark$top
  }

  if ( !is.null(mark$left) ) {
    anchor_left <- mark$left
  } else {
    anchor_left <- mark$parent$limits$width() - mark$right
  }

 anchor_left <- anchor_left + mark$parent$limits$left()
 anchor_bottom <- anchor_bottom +  mark$parent$limits$top()

  qfont(canvas) <- qfont(family = mark$font, pointsize = mark$pointsize, italic = mark$italic)
  qstrokeColor(canvas) <- mark$stroke
  qdrawText(canvas, text = mark$text, x = anchor_left, y = anchor_bottom, valign = mark$valign, halign = mark$halign, rot=mark$rot)
}

update.cranvas <- function(object, ...) {
new <- list(...)
structure(defaults(new, object), class = class(object))
}

###End Draw Wrappers###
##########################################################
##########################################################
####Canvas & Layers###

#-------------
#Create a blank canvas
new_plot <- function(width = 600, height = 400, xrange = c(0, 1), yrange = c(0, 1)) {
  size <- qsize(as.integer(c(width, height)))
  limits <- qrect(xrange, yrange)
  scene <- qscene()
  root <- qlayer(scene)
  root$setGeometry(qrect(0, 0, width, height))

#  root$setMaximumHeight(height)
#  root$setMaximumWidth(width)
  self <- structure(list(scene=scene,root=root,limits=limits, size = size), class = "cranvas-plot")
  self
}
#End blank canvas

#----------
#add layers to the canvas
add_layer<-function(  parent,
                      mark,
                      keyPressFun = NULL,
                      keyReleaseFun = NULL,
                      mouseDoubleClickFun = NULL,
                      mouseMoveFun = NULL,
                      mousePressFun = NULL,
                      mouseReleaseFun = NULL,
                      wheelFun = NULL,
                      hoverMoveFun = NULL,
                      hoverEnterFun = NULL,
                      hoverLeaveFun = NULL,
                      contextMenuFun = NULL,
                      dragEnterFun = NULL,
                      dragLeaveFun = NULL,
                      dragMoveFun = NULL,
                      dropFun = NULL,
                      focusInFun = NULL,
                      focusOutFun = NULL,
                      sizeHintFun = NULL,
                      row=0L,col=0L,
                      userlimits=NULL,
                      geometry = qrect(0,0,600,400),
                      clip = F,
                      colspan = 1L,
                      rowspan = 1L,
                      cache = F ){

  if(class(mark)[1]=="function"){
    paintFun<-mark
  }else{
    paintFun<-function(item, painter) { draw(mark, painter)}
  }

  if(is.null(userlimits)){
    limits<-parent$limits
  }else {
    limits<-userlimits
  }

  layer<- qlayer(  parent=parent$root,
                   paintFun=paintFun,
                   keyPressFun=keyPressFun,
                   keyReleaseFun=keyReleaseFun,
                   mouseDoubleClickFun=mouseDoubleClickFun,
                   mouseMoveFun=mouseMoveFun,
                   mousePressFun=mousePressFun,
                   mouseReleaseFun=mouseReleaseFun,
                   wheelFun=wheelFun,
                   hoverMoveFun = hoverMoveFun,
                   hoverEnterFun = hoverEnterFun,
                   hoverLeaveFun = hoverLeaveFun,
                   contextMenuFun = contextMenuFun,
                   dragEnterFun = dragEnterFun,
                   dragLeaveFun = dragLeaveFun,
                   dragMoveFun = dragMoveFun,
                   dropFun = dropFun,
                   focusInFun = focusInFun,
                   focusOutFun = focusOutFun,
                   sizeHintFun = sizeHintFun,
                   clip = clip,
                   limits = limits,
                   row = row,
                   col = col,
                   geometry = geometry,
                   rowSpan = rowspan,
                   colSpan = colspan,
                   cache = cache)
  layer$setLimits(limits)
  self<- structure(list(layer=layer, limits = limits))
  self
}
#End add_layer

#--------------------------------
#modify a layer
modify_layer<-function(	layerID,
                        parent,
                        show=NULL, #true or false
                        enabled=NULL, #true (allows interaction) or false (no interaction, faded appearance)
                        alpha=NULL, #ranges from 0.0(transparent) to 1.0 (opaque)
                        shift_right=NULL, #in canvas coordinates, see width above
                        shift_down=NULL #in canvas coordinates, see height above
){
  if(!is.null(show) && show==TRUE){
    parent$root$childItems()[[layerID]]$show()
  }else if (!is.null(show) && show==F){
    parent$root$childItems()[[layerID]]$hide()
  }

  if(!is.null(enabled)){
    parent$root$childItems()[[layerID]]$setEnabled(enabled)
  }

  if(!is.null(alpha)){
    parent$root$childItems()[[layerID]]$setOpacity(alpha)
  }

  if(!is.null(shift_right)){
    parent$root$childItems()[[layerID]]$setX(shift_right)
  }

  if(!is.null(shift_down)){
    parent$root$childItems()[[layerID]]$setY(shift_down)
  }
}
# End modify_layer





