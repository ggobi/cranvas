library(cranvas)
data(tennis)
qtennis = qdata(tennis, color = Aces)

## FIXME: this is not a good example; better use scatter plot
qparallel(10:14)  # variables including Aces

## now notice the color changes in the plot
color_pal(qtennis) = div_gradient_pal()  # change to diverging gradient palette

color_var(qtennis) = 'First.Serves'  # change color variable from Aces to First.Serves

color_label(qtennis) = 'First Serves'  # change label; to be used in legend
