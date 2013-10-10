# helpr function to save cranvas plots to png in knitr
qsave_knit = function(before, options, envir) {
  library(knitr)
  if (before || options$fig.num == 0L || !length(options$view)) return()
  my_save = function(name, view) {
    qsave(name, view, width = options$dpi * options$fig.width, 
        height = options$dpi * options$fig.height)
  }
  n = options$fig.num; view = options$view
  if (n == 1L) my_save(fig_path('.png', options), get(view)) else {
    for (i in seq(n)) {
      name = my_save(fig_path(paste(i, '.png', sep = ''), options), get(view[i]))
    }
  }
  hook_plot_custom(before, options, envir)
}
