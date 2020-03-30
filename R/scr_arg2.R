#' Argument 2: co-occurrence with 3 species
#'
#' Code to reproduce analysis for argument 2 and figure 2.
#'
#' @param nval Number of values used as x values.
#'
#' @export

scr_arg2 <- function(nval = 100) {
  # P(V1)
  seq_V1 <- seq(0.01, 0.99, length = nval)
  # P(V2)
  seq_V2 <- c(.05, .5, .95)
  # P(H|V2)
  seq_HV2 <- c(0, 0.2, 0.5, 0.95)

  # Co-occurrence signals
  ls_res <- list()
  for (k in seq_along(seq_HV2)) {
    mat1 <- matrix(0, nval, length(seq_V2))
    for (i in seq_len(nval)) {
      for (j in seq_along(seq_V2)) {
        tmp_pred <- sim_sp3(seq_V1[i], seq_V2[j], max(seq_HV2[k], 0.75), 0.75,
          seq_HV2[k], 0)
          mat1[i, j] <- tmp_pred[1L]
          ls_res[[k]] <- mat1
      }
    }
  }

  # global min and global max
  mx <- max(unlist(ls_res))
  mn <- min(unlist(ls_res))


  # ----- Figure 2 -----

  # output dir
  output_dir()

  # Line width for species association graph
  vc_lwd <- c(0, 1, 4, 7.5)
  mat_lay <- rbind(cbind(10, matrix(1:8, 4, byrow = TRUE)), c(0, 9, 0))

  png("output/fig2.png", res = 600, width = 4, height = 7.2, units = "in")

  layout(mat_lay, heights = c(rep(1, 4) , .3, .6), widths = c(.26, 1, .6))

  for (i in seq_len(4)) {
    par(mar = c(1.1, 1.5, .5, .0), mgp = c(2,.8,0), las = 1)
    ##
    plot0(c(0, 1), c(0, 0.2))
    lines(seq(0, 1, length.out = nval), ls_res[[i]][, 1L],
      col = pal[1], lwd = 5.4)
    lines(seq(0, 1, length.out = nval), ls_res[[i]][, 2L],
      col = pal[2], lwd = 3.6)
    lines(seq(0, 1, length.out = nval), ls_res[[i]][, 3L],
      col = pal[3],  lwd = 1.8)
    axis(2)
    if (i == 4) axis(1) else axis(1, at = seq(0, 1, .2), labels = rep("", 6))
    box(lwd = 1.1)
    mtext(letters[i], 3, line = -1.5, at = 0.02, cex = 1)
    ##
    par(mar = c(1, 1, .5, 1))
    plot_net_3(vc_lwd[i], rep("white", 3))
  }
  ##
  cx_txt <- 1.8
  par(mar = rep(.5, 4))
  plot0()
  text(0, -.2, labels = expression(P(X[V1])), cex = cx_txt)
  ##
  plot0()
  text(-.3, 0, labels = "Co-occurrence signal", cex = cx_txt, srt = 90)
  dev.off()

  msgSuccess_fig(2)

  invisible(NULL)
}


#============ NOT EXPORTED =============
# Compute correlations for three species
# in the 2 resources 1 consumer case
#=======================================
# p1 = P(V1)
# p2 = P(V2)
# a1 = P(H | V1 & 2)
# a2 = P(H | 1 only)
# a3 = P(H | 2 only)
# a4 = P(H | neither 1 nor 2)
# For this argument, we consider that H that feeds exclusively on V1 and V2,
# thus a4 = 0.

sim_sp3 <- function(p1, p2, a1, a2, a3, a4 = 0) {

  # presence of H
  pH <- a1 * p1 * p2 + a2 * p1 * (1 - p2) + a3 * (1 - p1) * p2 +
    a4 * (1 - p1) * (1 - p2)

  # used before
  # cor23 <- cooc_signal(p2, p3, p2 * (a1 * p1 + a3 * (1 - p1)))
  cor13 <- cooc_signal(p1, pH, p1 * (a1 * p2 + a2 * (1 - p2)))
}

# Co-occurrence signal
cooc_signal <- function(p1, p2, p12) {
  p12 - p1 * p2
}

plot_net_3 <- function(lwd_C2 = 0, vc_col = 1:3) {
  plot0(c(.5, 2.5), c(0.5, 2.5))
  lines(c(1, 1.5), c(1, 2), lwd = 4, col = "gray70")
  lines(c(2, 1.5), c(1, 2), lwd = lwd_C2, col = "gray70")
  points(c(1:2, 1.5), c(1, 1, 2), pch = 21, bg = vc_col, cex = 5, lwd = 2.2)
  text(c(1:2, 1.5), c(1, 1, 2), labels = c(paste0("V", 1:2), "H"), cex = 1.4, col = "grey10")
  invisible(NULL)
}

