#' Argument 1: environment and occurrence
#'
#' Code to reproduce analysis for 1st argument and figure 1.
#'
#' @export


scr_arg1 <- function() {
  denv <- 0.01
  seq_env <- seq(0, 5, denv)
  ## Presence vs Environment
  prob1 <- logistic2(seq_env - 2.5, yzer = 0.5, ypos = 0.01, yneg = 0.99,
    lambda = 2)
  prob2 <- logistic2(seq_env - 2.5, yzer = 0.5, yneg = 0.01, ypos = 0.99,
    lambda = 2)
  # Environemnt
  env <- rep(0, length(seq_env))
  env[seq_env >= 2 & seq_env <= 3] <- 1

  ls_envi <- list(env)
  ls_prob <- list(prob1 * prob2 * env * denv)
  ls_sprob <- lapply(ls_prob, sum)

  # output dir
  output_dir()

  mat_lay <- cbind(rbind(rep(1, 4), rep(2:3, each = 2), rep(4:5, each = 2), 6))

  png("output/fig1.png", res = 300, width = 6, height = 5, units = "in")
  layout(mat_lay, heights = c(0.5, 1, 1, 0.15), widths = rep(1, 6))

  ## Picture
  par(mar = c(1, 2, 1, 1), cex.axis = 1, cex.lab = 1.1, mgp = c(3.2, 1, 0))
  pic <- "inst/img/Mont_Megantic_MarkVellend.jpg"
  if (file.exists(pic)) {
    plotImage(file = pic)
  } else plot0(text = "Photo not available.")
  mlet()

  ##
  par(las = 1, mar = c(2, 5, 1, 1))
  plot0(range(seq_env), c(0, 1))
  lines(seq_env, prob1, col = pal[1], lwd = 2)
  lines(seq_env, prob2, col = pal[2], lwd = 2)
  text(c(0.8, 4.2), c(0.8, 0.8), labels = c(expression(P(X[A] * "|" * E)),
    expression(P(X[B] * "|" * E))), col = pal, cex = 1.4)

  axis(1, lwd = 0, lwd.ticks = 0.5)
  axis(2, lwd = 0, lwd.ticks = 0.5)
  box2(1:2, lwd = 1.2)
  title(ylab = "Occurrence probability", cex.lab = 1.5)
  mlet("b", line = -0.4)

  ##
  par(las = 1, mar = c(2, 7, 1, 1))
  plot0(range(seq_env), c(0, 0.25))
  lines(seq_env, prob1 * prob2, col = "black", lwd = 2)
  axis(1, lwd = 0, lwd.ticks = 0.5)
  axis(2, lwd = 0, lwd.ticks = 0.5)
  par(xpd = TRUE)
  title(ylab = expression(P(X[A] * "," * X[B] * "|" * E)), cex.lab = 1.5, line = 4)
  par(xpd = TRUE)
  box2(1:2, lwd = 1.2)
  mlet("c", line = -0.4)

  par(mar = c(2, 5, 2, 1))
  # may be a loopagin
  i = 1
  plot0(range(seq_env), c(0, 1))
  envelop(seq_env, ls_envi[[i]], col = "grey90", border = NA)
  lines(seq_env, ls_envi[[i]], lwd = 0.5)
  axis(1, lwd = 0, lwd.ticks = 0.5)
  axis(2, lwd = 0, lwd.ticks = 0.5)
  title(ylab = expression(P(E)), cex.lab = 1.5)

  box2(1:2, lwd = 1.2)
  mlet(let = letters[3 + i])

  ##
  par(mar = c(2, 7, 2, 1))
  plot0(range(seq_env), range(ls_prob[[i]]))
  lines(seq_env, ls_prob[[i]])
  axis(1, lwd = 0, lwd.ticks = 0.5)
  axis(2, lwd = 0, lwd.ticks = 0.5)
  title(ylab = expression(P(X[A] * "," * X[B] * "|" * E) * P(E)),
    cex.lab = 1.5, line = 4)
  box2(1:2, lwd = 1.2)
  mtext(expression(P(X[A] * "," * X[B])), 3, cex = 0.8, at = 2.5, adj = 1)
  mtext(paste0(" = ", format(ls_sprob[[i]], digits = 3)), 3, cex = 0.76,
    at = 2.5, line = 0.4, adj = 0)
  mlet(let = letters[4 + i])

  ##
  par(mar = c(0, 0, 0, 0))
  plot0()
  text(0, 0, labels = "Environmental gradient (e.g. elevation)", cex = 1.5)

  dev.off()

  invisible(NULL)
}

# layout 6 fig mat_lay <- cbind( c(1, 2, 6, 0), rbind(rep(1, 6), rep(2:3, each =
# 3), rep(4:5, each = 3), 7) )
