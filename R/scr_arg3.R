#' Argument 3: co-occurrence and indirect association
#'
#' Code to reproduce analysis for 1st argument and figure 3.
#'
#' @export

scr_arg3 <- function() {

  seq_x <- seq(0.01, 0.99, 0.01)
  val <- do.call(rbind, lapply(seq_x, function(x) sim_RCT(0.5, x, 0.5, 0.5)))
  vc_txt <- c("R", "C", "P", "T")


  # output dir
  output_dir()

  png(filename = "output/fig3.png", width = 5.5, height = 5, res = 600, units = "in")

  layout(matrix(1:2, 1), widths = c(1, 0.25))

  par(mar = c(4.2, 4.2, 1.5, 0.5), las = 1, mgp = c(3, 1, 0), cex.lab = 1.2, cex.axis = 1.2)

  plot0(c(0, 1), c(0, 0.25))
  axis(2)
  axis(1)
  box2(1:2, lwd = 2)
  title(xlab = expression(P(X[C] * "|" * X[R])), ylab = "Co-occurrence signal")
  for (i in seq_len(3)) {
    lines(seq_x, val[, i], col = pal[i + 1], lwd = 3.6)
  }
  mtext(3, adj = 0.01, text = "a", cex = 1.4, line = -1)

  par(mar = c(3, 1, 1.5, 1))
  plot0(c(-1, 1), c(-1, 9.8))
  lines(c(0, 0), c(0, 9), lwd = 6)
  for (i in seq_len(4)) {
    points(0, (i - 1) * 3, pch = 21, bg = pal[i], cex = 5,
      col = darken(pal[i], 40), lwd = 2.5)
    text(0, (i - 1) * 3, labels = vc_txt[i], col = contrastColors(pal[i]))
  }
  mtext(3, adj = 0.01, text = "b", cex = 1.4, line = -1)

  dev.off()

  success_msg_fig(3)

  invisible(NULL)

}



# Compute all correlations for three species R -> C -> T
# in the 2 resources 1 consumer case:
# pR = prob presence Resource
# aRC = prob presence of the consumer given the resource
# aCT = prob presence of the top-predator given the consumer
# aRT = prob presence of the top-predator given resource without consumer
# NB we assume prob presence of the consumer without resource is 0, same for top
# pred with teh consumer
sim_RCT <- function(pR, aRC, aCP, aPT) {
  pC <- aRC * pR  # also P(C,R)
  pP <- aCP * pC  # also P(P,R)
  pT <- aPT * pP  # also P(T,R)
  c(pC * (1 - pR), pP * (1 - pR), pT * (1 - pR))
}

# -- old way
# sim_RCT <- function(pR, aRC, aCP, aPT) {
#   pC <- aRC*pR
#   pP <- aCP*pC
#   pT <- aPT*pP
#   corTP <- cor_theo(pT, pP, pT)
#   corTC <- cor_theo(pT, pC, pT)
#   corTR <- cor_theo(pT, pR, pT)
#   # c(pR, pC, pT)
#   c(corTR, corTC, corTP)
# }