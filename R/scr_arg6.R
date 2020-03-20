#' Argument 6: co-occurrence and co-existence
#'
#' Code to reproduce analysis for 1st argument and figure 6.
#'
#' @param nval Number of interactions strength values.
#'
#' @export


scr_arg6 <- function(nval = 200) {

  sig_vec <- seq(0, 1, length.out = nval)
  res <- matrix(nrow = nval, ncol = 5)
  pars <- list(a1 = 1, a2 = 1, f1 = 1, fp1 = 1, s1 = 5, sp1 = 1, e1 = 1,
    ep1 = 1, f2 = 1, fp2 = 1, s2 = 4.5, sp2 = 1, e2 = 1, ep2 = 1)

  for (i in seq_len(nval)) {
    pars$sp1 <- pars$s1 * (1 - pars$a1 * sig_vec[i])
    pars$sp2 <- pars$s2 * (1 - pars$a2 * sig_vec[i])
    eq <- runeq(pars)
    p1 <- eq[1L]
    p2 <- eq[2L]
    p12 <- eq[3L]
    pIND <- (p1 + p12) * (p2 + p12)
    res[i, ] = c(p1, p2, p12, pIND, p12/pIND)
  }


  # output dir
  output_dir()

  png("output/fig6.png", res = 600, width = 6, height = 4.5, units = "in")
  par(oma = c(2, 0, 0, 0), mar = c(1, 5, 0.5, 0.5), lend = 1)

  layout(c(1, 2), heights = c(1, 0.7))

  plot(sig_vec, res[, 1] + res[, 3], ylim = c(0, 1), xlab = "", type = "l", col = "orange",
    lwd = 3, ylab = NA, las = 1, cex.lab = 1.5, axes = FALSE)
  box()

  axis(1, las = 1, at = seq(0, 1, by = 0.2), labels = NA, cex.axis = 0.75)
  axis(2, las = 1, cex.axis = 0.75)
  mtext(side = 2, line = 3, "Occurrence probability", cex = 1)
  lines(sig_vec, res[, 2] + res[, 3], col = "blue", lwd = 3)
  abline(h = 0.5, lty = 3, lwd = 1)

  legend("bottomleft", col = c("orange", "blue"), legend = c("Species A", "Species B"),
    lty = 1, cex = 1, lwd = 3, bty = "n")
  text(-0.02, 0.98, "a")


  plot(sig_vec, res[, 3] - res[, 4], type = "l", ylim = c(-0.2, 0.2), lwd = 3,
    xlab = "", ylab = "Spatial association", axes = FALSE, cex.lab = 1, las = 1)
  box()

  abline(h = 0, lty = 3, lwd = 1)
  axis(1, las = 1, at = seq(0, 1, by = 0.2), cex.axis = 0.75)
  axis(2, las = 1, cex.axis = 0.75)

  mtext(side = 1, line = 1, text = "Interaction strength", outer = TRUE)
  text(-0.02, 0.18, "b")

  dev.off()

  success_msg_fig(6)

  invisible(NULL)
}



# ============ NOT EXPORTED =============

# Model definition
model <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {

    p0 <- 1 - p1 - p2 - p12

    dp1dt <- f1 * s1 * p0 * p1 + fp1 * s1 * p0 * p12 - e1 * p1 + ep2 * p12 -
      (f2 * p2 + fp2 * p12) * sp2 * p1
    dp2dt <- f2 * s2 * p0 * p2 + fp2 * s2 * p0 * p12 - e2 * p2 + ep1 * p12 -
      (f1 * p1 + fp1 * p12) * sp1 * p2
    dp12dt <- (f1 * p1 + fp1 * p12) * sp1 * p2 + (f2 * p2 + fp2 * p12) * sp2 *
      p1 - (ep1 + ep2) * p12

    list(c(dp1dt, dp2dt, dp12dt))
  })
}

# Wrapper to solve the model to equilibrium
runeq <- function(pars) {
  # Initial conditions
  pstart <- c(p1 = 0.5, p2 = 0.5, p12 = 0)
  # Run the model to equilibrium
  stode(y = pstart, func = model, parms = pars, positive = TRUE)[[1L]]
}
