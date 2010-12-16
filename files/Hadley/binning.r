
pixbin2 <- function(x, y, n) {
  mat <- cbind(df$x, df$y)
  mat <- mat[complete.cases(mat), ]
  rng <- rbind(range(mat[ ,1]), range(mat[, 2]))

  subset(melt(bin2(mat, rng, n)$nc), value > 0)
}

pixbin3 <- function(x, y, n) {
  x <- cut(x, n[1], labels = FALSE)
  y <- cut(y, n[2], labels = FALSE)
  
  bin <- (x - 1) + (y - 1) * (n[1])

  bins <- tabulate(bin, n[1] * n[2])
  non_empty <- which(bins > 0)
  bins <- bins[bins > 0]
  
  data.frame(
    X1 = non_empty %% n[1] + 1,
    X2 = non_empty %/% n[1] + 1,
    value = bins
  )
}

pixbin4 <- function(x, y, n) {
  x <- cut2(x, n[1], labels = FALSE)
  y <- cut2(y, n[2], labels = FALSE)
  
  bin <- (x - 1) + (y - 1) * (n[1])

  bins <- tabulate(bin, n[1] * n[2])
  
  non_empty <- which(bins > 0)
  bins <- bins[bins > 0]
  
  data.frame(
    X1 = non_empty %% n[1] + 1,
    X2 = non_empty %/% n[1] + 1,
    value = bins
  )
}

cut2 <- function(x, breaks) {
  nb <- as.integer(breaks + 1)
  rx <- range(x, na.rm = TRUE)
  dx <- diff(rx)
  if (dx == 0) 
      dx <- abs(rx[1L])
  breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000, 
      length.out = nb)
      
  findInterval(x, breaks)
}


# library(ggplot2)
# df <- data.frame(x = diamonds$carat, y = diamonds$price)
# system.time(pixbin2(df$x, df$y, c(500, 500)))
# system.time(pixbin3(df$x, df$y, c(500, 500)))

# load("geo.rdata")  
# df <- data.frame(y = geo$lat * 100, x = geo$long * 100)
# df <- df[complete.cases(df), ]
# system.time(pixbin3(df$x, df$y, c(500, 500)))

# system.time(pixbin3(geo$lat, geo$long, c(500, 500)))



