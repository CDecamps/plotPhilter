#'Density heatmap 
#'
#'Density heatmap of the relationship between two variables
#' 
#'@author Florent Chuffar
#'
#'@export
#'
#'
custom_hm = function(x, y, xlim, ylim, xlab, ylab, main = "", col="darkgrey", ncol=100, nbin=50, LAYOUT=TRUE, LOGCOUNT=FALSE, wrapper_func, letter) {
  if(missing(xlim)){
    xlim = c(min(x), max(x))
  }
  if(missing(ylim)){
    ylim = c(min(y), max(y))
  }
  
  z = MASS::kde2d(x, y, n=nbin, lims=c(xlim, ylim)) # Just for fun??
  
  x_breaks = seq(from=min(xlim), to=max(xlim), length=nbin+1)
  x_breaks[length(x_breaks)] = max(x)
  x_breaks[1] = min(x)
  
  y_breaks = seq(from=min(ylim), to=max(ylim), length=nbin+1)
  y_breaks[length(y_breaks)] = max(y)
  y_breaks[1] = min(y)
  
  x_bins <- cut(x, breaks=x_breaks, include.lowest=TRUE)
  y_bins <- cut(y, breaks=y_breaks, include.lowest=TRUE)
  table_2d <- table(x_bins, y_bins)
  count_matrix <- as.matrix(table_2d)
  if (LOGCOUNT) {
    count_matrix = log(count_matrix+1)
  }
  z$z = count_matrix # Not really!!
  
  col_drk = col
  col_gradient = colorRampPalette(c("white", col_drk))(ncol)[-1]
  if (LAYOUT) {
    layout(matrix(c(
      3,3,3,3,4,
      1,1,1,1,2,
      1,1,1,1,2,
      1,1,1,1,2,
      1,1,1,1,2,
      NULL), 5, byrow=TRUE), respect=TRUE)
  }
  
  par(mar=c(5.1, 4.1, 0, 0))
  graphics::image(z, xlab=xlab, ylab=ylab, useRaster=TRUE, col=col_gradient, frame.plot=TRUE)
  # axis(1, at=c(0, 0.5, 1), labels=sort(c(0, xlim)))
  # if (!missing(wrapper_func)) {
  #     wrapper_func()
  # }
  
  par(mar=c(5.1, 0, 0, .1))
  par(xpd = NA)
  plot(apply(z$z, 2, sum),z$y, type="l", frame.plot=FALSE, yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="", col=col, lwd=3)
  par(xpd = FALSE)
  
  par(mar=c(0, 4.1, 4.1, 0))
  par(xpd = NA)
  plot(z$x,apply(z$z, 1, sum), type="l", frame.plot=FALSE, xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="", main=main, col=col, lwd=3) 
  # if (!missing(letter)) {
  #   put_a_letter(letter)
  # }
  par(xpd = FALSE)
  
  par(mar=c(2.1, 0.5, 2.1, 2.1))
  # plot(density(z$z), xlab="", ylab="", main="", yaxt="n", frame.plot=FALSE)
  # graphics::image(z$z, xlab=xlab, ylab=ylab, useRaster=TRUE, col=col, frame.plot=FALSE)
  graphics::image(matrix(1:ncol), col=col_gradient, useRaster=TRUE, frame.plot=FALSE, yaxt="n", xaxt="n")
  at = seq(from=-.5/ncol, to=1+(.5/ncol), length.out=5)
  labels = seq(from=min(z$z), to=max(z$z), length.out=5)
  if (LOGCOUNT) {
    labels = exp(labels)-1
  }
  labels = signif(round(labels),3)
  # labels = c(, round(mean(range(z$z))), ceiling(max(z$z)))
  axis(1, at=at, labels=labels, las=2, cex.axis=.8)
  
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  layout(1)
}
