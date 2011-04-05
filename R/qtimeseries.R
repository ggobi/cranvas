#setwd('/home/xiaoyue/Cranvas')

library(qtbase)
library(qtpaint)
library(plumbr)
library(cranvas)

qts <- function(time, y, data) {
    tmpdata <- mutaframe(time = data[, time], zoomgroup = rep(1, nrow(data)))
    cy <- y[1]
    brush_mouse_press <- function(layer, event) {
        .bstart <<- as.numeric(event$pos())
        if (event$button() == Qt$Qt$RightButton) {
            .bmove <<- FALSE
        }
        if (event$button() == Qt$Qt$LeftButton) {
            .bmove <<- TRUE
        }
    }
    brush_mouse_release <- function(layer, event) {
        .bend <- as.numeric(event$pos())
        idx <- tmpdata$time > min(.bstart[1], .bend[1]) & data[, cy] > min(.bstart[2], 
            .bend[2]) & tmpdata$time < max(.bstart[1], .bend[1]) & data[, cy] < max(.bstart[2], 
            .bend[2])
        selected(data) <- idx
        if (length(idx)) 
            qupdate(brush_layer)
    }
    mouse_wheel <- function(layer, event) {
        # tmp<<-event
        # print(event$type())
    }
    key_press <- function(layer, event) {
        if (event$key() == Qt$Qt$Key_Plus) {
            zoombound = round(0.8 * max(tmpdata$time))
            if (zoombound < 3) 
                zoombound <- 3
            tmpdata$time <- data[, time]%%zoombound
            tmpdata$zoomgroup <- ceiling(data[, time]/zoombound)
            if (sum(tmpdata$time == 0)) {
                tmpdata$time[tmpdata$time == 0] <- zoombound
                tmpdata$zoomgroup[tmpdata$time == 0] <- tmpdata$zoomgroup[which(tmpdata$time == 
                  0) - 1]
            }
        }
        if (event$key() == Qt$Qt$Key_Minus) {
            zoombound = round(1.25 * max(tmpdata$time))
            if (zoombound > max(data[, time])) 
                zoombound <- max(data[, time])
            tmpdata$time <- data[, time]%%zoombound
            tmpdata$zoomgroup <- ceiling(data[, time]/zoombound)
            if (sum(tmpdata$time == 0)) {
                tmpdata$time[tmpdata$time == 0] <- zoombound
                tmpdata$zoomgroup[tmpdata$time == 0] <- tmpdata$zoomgroup[which(tmpdata$time == 
                  0) - 1]
            }
        }
        main_layer$setLimits(qrect(range(tmpdata$time), range(data[, cy])))
        brush_layer$setLimits(qrect(range(tmpdata$time), range(data[, cy])))
        qupdate(main_layer)
        qupdate(xaxis)
    }
    main_draw <- function(layer, painter) {
        color = gray(seq(0, 0.8, length = max(tmpdata$zoomgroup)))
        for (i in 1:max(tmpdata$zoomgroup)) {
            qdrawCircle(painter, tmpdata[tmpdata$zoomgroup == i, 1], data[tmpdata$zoomgroup == 
                i, cy], 2, fill = color[i], stroke = color[i])
            qdrawLine(painter, tmpdata[tmpdata$zoomgroup == i, 1], data[tmpdata$zoomgroup == 
                i, cy], stroke = color[i])
        }
    }
    brush_draw <- function(layer, painter) {
        idx <- selected(data)
        if (any(idx)) {
            txt <- paste("Point ", rownames(data)[idx], "\nTime: ", data[idx, time], 
                "\nValue: ", data[idx, cy], sep = "")
            qdrawText(painter, txt, tmpdata[idx, 1], data[idx, cy])
        }
    }
    scene <- qscene()
    root_layer <- qlayer(scene)
    xaxis <- qaxis(root_layer, data = tmpdata$time, side = 1, limits = range(tmpdata$time), 
        row = 2, col = 1)
    yaxis <- qaxis(root_layer, data = data[, cy], side = 2, limits = range(data[, 
        cy]), row = 1, col = 0)
    main_layer <- qlayer(root_layer, paintFun = main_draw, mousePressFun = brush_mouse_press, 
        mouseReleaseFun = brush_mouse_release, wheelFun = mouse_wheel, keyPressFun = key_press, 
        limits = qrect(range(tmpdata$time), range(data[, cy])), row = 1, col = 1)
    brush_layer <- qlayer(root_layer, brush_draw, limits = qrect(range(tmpdata$time), 
        range(data[, cy])), row = 1, col = 1)
    layout = root_layer$gridLayout()
    layout$setRowPreferredHeight(0, 30)
    layout$setColumnPreferredWidth(0, 30)
    layout$setRowPreferredHeight(2, 30)
    layout$setColumnMaximumWidth(2, 10)
    layout$setRowStretchFactor(0, 0)
    layout$setColumnStretchFactor(0, 0)
    layout$setRowStretchFactor(2, 0)
    view <- qplotView(scene = scene)
    view
} 
