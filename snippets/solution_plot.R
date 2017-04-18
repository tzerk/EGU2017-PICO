solution_plot <- function(data, cex, ratio, rotate, kde, hist, dots, code = FALSE) {
  
  cex <- eval(cex)
  ratio <- eval(ratio)
  rotate <- eval(rotate)
  kde <- eval(kde)
  hist <- eval(hist)
  dots <- eval(dots)
  
  ## now with some graphical modification
  fun <- deparse(width.cutoff = 20, substitute(
    plot_AbanicoPlot(data = data, na.rm = TRUE, log.z = TRUE, 
                     z.0 = "median", 
                     col = c("steelblue4", "orange4"),
                     bar.col = c("steelblue3", "orange3"),
                     polygon.col = c("steelblue1", "orange1"),
                     pch = c(2, 6),
                     angle = c(30, 50),
                     summary = c("n", "sd.rel", "sd.abs", "kurtosis", "skewness"), 
                     summary.pos = "bottomleft",
                     main = "A fancy plot",
                     dispersion = "p05",
                     plot.ratio = ratio, 
                     rotate = rotate,
                     mtext = "", 
                     rug = TRUE,
                     summary.method = "MCM", 
                     legend = c("data set 1", "data set 2"), 
                     legend.pos = "bottomright", stats = c("min", "max"), 
                     kde = kde, 
                     hist = hist,
                     dots = dots, 
                     boxplot = TRUE, 
                     y.axis = TRUE, 
                     error.bars = FALSE, 
                     bar = TRUE, 
                     frame = 1, 
                     grid.col = FALSE, 
                     bw = 0.08, 
                     output = FALSE, 
                     interactive = FALSE, line = 19.7, line.col = "red", line.lty = 2, line.label = "outlier!?",
                     xlab = c("Relative standard error (%)", "Precision"),
                     zlab = "De [Gy]",
                     ylab = "Standardised estimate",
                     cex = cex)
  ))
  
  if (!code)
    eval(parse(text = fun))
  else
    return(fun)
}

