ccoh2 <- function (wave1, wave2, f, wl = 512, ovlp = 0, plot = TRUE, grid = TRUE, 
          scale = TRUE, cont = FALSE, collevels = seq(0, 1, 0.01), 
          palette = reverse.heat.colors, contlevels = seq(0, 1, 0.01), 
          colcont = "black", colbg = "white", colgrid = "black", colaxis = "black", 
          collab = "black", xlab = "Time (s)", ylab = "Frequency (kHz)", 
          scalelab = "Coherence", main = NULL, scalefontlab = 1, scalecexlab = 0.75, 
          axisX = TRUE, axisY = TRUE, flim = NULL, flimd = NULL, ...) 
{
  input1 <- inputw(wave = wave1, f = f)
  wave1 <- input1$w
  f <- input1$f
  rm(input1)
  wave2 <- inputw(wave = wave2, f = f)$w
  n1 <- nrow(wave1)
  n2 <- nrow(wave2)
  if (n1 != n2) 
    stop("'wave 1' and 'wave 2' must have the same length")
  n <- n1
  if (!is.null(flimd)) {
    mag <- round((f/2000)/(flimd[2] - flimd[1]))
    wl <- wl * mag
    if (ovlp == 0) 
      ovlp <- 100
    ovlp <- 100 - round(ovlp/mag)
    flim <- flimd
  }
  step <- seq(1, n + 1 - wl, wl - (ovlp * wl/100))
  z1 <- matrix(data = numeric((wl) * length(step)), wl, length(step))
  for (i in step) {
    z1[, which(step == i)] <- spec.pgram(cbind(wave1[i:(wl + 
                                                          i - 1), ], wave2[i:(wl + i - 1), ]), spans = c(3, 
                                                                                                         3), fast = FALSE, taper = 0.1, plot = FALSE)$coh
  }
  z <- z1[1:(wl/2), ]
  X <- seq(0, n/f, length.out = length(step))
  if (is.null(flim)) {
    Y <- seq(0, f/2000, length.out = nrow(z))
  }
  else {
    fl1 <- flim[1] * nrow(z) * 2000/f
    fl2 <- flim[2] * nrow(z) * 2000/f
    z <- z[fl1:fl2, ]
    Y <- seq(flim[1], flim[2], length.out = nrow(z))
  }
  Z <- t(z)
  if (plot) {
    Zlim <- range(Z, finite = TRUE)
    if (scale) {
      def.par <- par(no.readonly = TRUE)
      on.exit(par(def.par))
      layout(matrix(c(1, 2), ncol = 2, byrow = TRUE), widths = c(6, 
                                                                 1))
      par(mar = c(5, 4.1, 1, 0), las = 1, cex = 1, bg = colbg, 
          col = colaxis, col.axis = colaxis, col.lab = collab)
      filled.contour.modif2(x = X, y = Y, z = Z, levels = collevels, 
                            nlevels = 20, plot.title = title(main = main, 
                                                             xlab = xlab, ylab = ylab), color.palette = palette, 
                            axisX = axisX, axisY = axisY)
      if (grid) 
        grid(nx = NA, ny = NULL, col = colgrid)
      if (colaxis != colgrid) 
        abline(h = 0, col = colaxis)
      else abline(h = 0, col = colgrid)
      par(mar = c(5, 1, 4.5, 3), las = 0)
      dBscale(collevels = collevels, palette = palette, 
              fontlab = scalefontlab, cexlab = scalecexlab, 
              collab = collab, textlab = scalelab, colaxis = colaxis)
    }
    if (scale == FALSE) {
      par(las = 1, col = colaxis, col.axis = colaxis, col.lab = collab, 
          , bg = colbg, ...)
      filled.contour.modif2(x = X, y = Y, z = Z, levels = collevels, 
                            nlevels = 20, plot.title = title(main = main, 
                                                             xlab = xlab, ylab = ylab), color.palette = palette, 
                            axisX = axisX, axisY = axisY, col.lab = collab, 
                            colaxis = colaxis)
      if (grid) 
        grid(nx = NA, ny = NULL, col = colgrid)
      if (colaxis != colgrid) 
        abline(h = 0, col = colaxis)
      else abline(h = 0, col = colgrid)
    }
    if (cont) {
      contour(X, Y, Z, add = TRUE, levels = contlevels, 
              nlevels = 5, col = colcont, ...)
    }
    invisible(list(time = X, freq = Y, coh = Z))
  }
  else return(list(time = X, freq = Y, coh = Z))
}
