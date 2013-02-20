library(cranvas)
qtennis = qdata(tennis, color = aces)

qscatter(server.pts, first.serves)
qparallel(10:14)  # variables including Aces
qscatter(server.pts, aces)


## now notice the color changes in the plot
color_pal(qtennis) = div_gradient_pal()  # change to diverging gradient palette

color_var(qtennis) = 'first.serves'  # change color variable from Aces to First.Serves

color_title(qtennis) = 'First Serves'  # change title; to be used in legend
