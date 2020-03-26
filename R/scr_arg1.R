#' Argument 1: environment and occurrence
#'
#' Code to reproduce analysis for 1st argument and figure 1.
#'
#' @param denv incremental value for the environmental gradient.
#'
#' @export

scr_arg1 <- function(denv = 0.01) {

  seq_env <- seq(0, 5, denv)
  ## Presence vs Environment
  prob1 <- logistic2(seq_env - 2.5, yzer = 0.5, ypos = 0.01, yneg = 0.99,
    lambda = 2)
  prob2 <- logistic2(seq_env - 2.5, yzer = 0.5, yneg = 0.01, ypos = 0.99,
    lambda = 2)

  # Environmental gradients
  env0 <- env1 <- env2 <- env3 <- rep(0, length(seq_env))
  env1[seq_env >= 0 & seq_env <= 5] <- 0.2
  env2[seq_env >= 1.5 & seq_env <= 3.5] <- .5
  env3[seq_env >= 0 & seq_env <= 2.5] <- seq(.8, 0,
      length.out = .5*length(env0))

  ls_envi <- list(env1, env2, env3)
  ls_prob <- list(
    prob1 * prob2 * env1 * denv,
    prob1 * prob2 * env2 * denv,
    prob1 * prob2 * env3 * denv)
  ls_sprob <- lapply(ls_prob, sum)


  # output dir
  output_dir()

  mat_lay <- cbind(c(1, 2, 10, 11, 0), rbind(rep(1, 6), rep(2:3, each = 3), matrix(rep(4:9, each = 2), byrow = TRUE, nrow = 2), 12))

  png("output/fig1.png", res = 600, width = 6, height = 7, units = "in")
  layout(mat_lay, heights = c(0.5, 1, 0.8, 0.8, 0.15), widths = c(0.6, rep(1, 6)))

  ## Picture
  par(mar = c(1, 2, 1, 1), cex.axis = 1, cex.lab = 1.1, mgp = c(3.2, .8, 0))
  pic <- "inst/img/Mont_Megantic_MarkVellend.jpg"
  if (file.exists(pic)) {
    plotImage(file = pic)
  } else plot0(text = "Photo not available.")
  mlet()


  ## Occurrence probability for A and B
  par(las = 1, mar = c(2, 6, 1, 3))
  plot0(range(seq_env), c(0, 1))
  lines(seq_env, prob1, col = pal[3], lwd = 2)
  lines(seq_env, prob2, col = pal[2], lwd = 2)
  text(c(0.8, 4.2), c(0.8, 0.8), col = pal[c(3, 2)], cex = 1.4,
    labels = c(expression(P(X[A] * "|" * E)), expression(P(X[B] * "|" * E))))

  axis(1, lwd = 0, lwd.ticks = 0.5)
  axis(2, lwd = 0, lwd.ticks = 0.5)
  box2(1:2, lwd = 1.2)
  title(ylab = "Occurrence probability", cex.lab = 1.2, mgp = c(3.6, 1, 0))
  mlet("b", line = -0.4)

  ## Co-occurrence probability
  par(las = 1, mar = c(2, 4, 1, 1))
  plot0(range(seq_env), c(0, 0.25))
  lines(seq_env, prob1 * prob2, col = 1, lwd = 2)
  axis(1, lwd = 0, lwd.ticks = 0.5)
  axis(2, lwd = 0, lwd.ticks = 0.5)
  title(ylab = expression(P(X[A] * "," * X[B] * "|" * E)), cex.lab = 1.2)
  box2(1:2, lwd = 1.2)
  mlet("c", line = -0.4)


  ## P(E) distributions - 3 scenarios
  par(mar = c(2.5, 1.5, 1.2, 1))
  for (i in 1:3) {
    plot0(range(seq_env), c(0, .8))
    envelop(seq_env, ls_envi[[i]], col = "grey90", border = NA)
    lines(seq_env, ls_envi[[i]], lwd = 0.8)
    axis(1, lwd = 0, lwd.ticks = 0.5)
    if (i == 1) {
      axis(2, lwd = 0, lwd.ticks = 0.5)
    } else {
      axis(2, at = seq(0, .8, 0.2), labels = rep("", 5), lwd = 0,
        lwd.ticks = 0.5)
    }
    box2(1:2, lwd = 1.2)
    mlet(let = letters[3 + i], line = -0.4)
  }

  ## Realized co-occurrence
  par(mar = c(2, 1.5, 1.6, 1))
  for (i in 1:3) {
    plot0(range(seq_env), c(0, 0.0015))
    envelop(seq_env, ls_prob[[i]], col = "grey90", border = NA)
    lines(seq_env, ls_prob[[i]], lwd = 0.8)
    axis(1, lwd = 0, lwd.ticks = 0.5)
    if (i == 1) {
      axis(2, lwd = 0, lwd.ticks = 0.5)
    } else {
      axis(2, at = seq(0, 0.0015, 5e-04), labels = rep("", 4), lwd = 0, lwd.ticks = 0.5)
    }
    box2(1:2, lwd = 1.2)
    mtext(expression(P(X[A] * "," * X[B])), 3, cex = 0.8, at = 2.5, adj = 1)
    mtext(paste0(" = ", format(ls_sprob[[i]], digits = 3)), 3, cex = 0.76,
      at = 2.5, line = 0.4, adj = 0)
    mlet(let = letters[6 + i])
  }

  par(mar = c(0, 0, 0, 0))
  plot0()
  text(-0.6, 0, "density", srt = 90, cex = 1.2)
  text(.2, 0, labels = expression(f(E)), srt = 90, cex = 1.2)
  ##
  plot0()
  text(-0.6, 0, labels = expression(P(X[A] * "," * X[B] * "|" * E) * f(E)),
    srt = 90, cex = 1.2)
  ##
  plot0()
  text(0, 0, labels = "Environmental gradient (e.g. elevation)", cex = 1.4)

  dev.off()

  msgSuccess_fig(1)

  invisible(NULL)

}



