library(cranvas)
data(tennis)
qtennis = qdata(tennis, color = Aces)

qscatter(Server.Pts, First.Serves)
qparallel(10:14)  # variables including Aces
qscatter(Server.Pts, Aces)


## now notice the color changes in the plot
color_pal(qtennis) = div_gradient_pal()  # change to diverging gradient palette

color_var(qtennis) = 'First.Serves'  # change color variable from Aces to First.Serves

color_title(qtennis) = 'First Serves'  # change title; to be used in legend
