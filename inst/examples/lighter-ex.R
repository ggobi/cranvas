lighter(palette())

par(mfrow = c(2, 1), mar = rep(0, 4))
d = c('black', grep('dark', colors(), value = TRUE))  # dark colors
x = rep(1, length(d))
for (f in seq(0, 1, length = 20)) {
    barplot(x, col = d); barplot(x, col = lighter(d, f))
    if(interactive()) Sys.sleep(.2)
}
