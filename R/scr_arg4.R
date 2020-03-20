#' Argument 4: co-occurrence and scale
#'
#' Code to reproduce analysis for argument 4 and figure 4. Use
#' `set.seed(7891)` and `nrep = 1e-5` for to obtain the results` found in the
#' original study.
#'
#' @param run_all Number of repetitions for every environmental values.
#' @param nrep Number of repetitions for every environmental values, ignored if
#' '`run_all = FALSE`.
#'
#' @export

scr_arg4 <- function(run_all = FALSE, nrep = 1e4) {

  # sequence size
  seq_env_d <- seq(0, 5, 0.01)
  seq_env <- seq(-.2, 5.2, 0.01)
  sz <- length(seq_env)
  # windows size
  vc_w_sz <- c(25, 50, 100, 200, 300, 400, 450, 475)
  # Presence Scenario 1 - A and B have different requirements
  occ_A_1 <- gaussianShape(seq_env, optx = 1.6, opty = 0.8, width = 1, pow = 2)
  occ_B_1 <- gaussianShape(seq_env, optx = 3.4, opty = 0.8, width = 1, pow = 2)
  ## Scenario 2 - A and B have similar requirements
  occ_A_2 <- gaussianShape(seq_env, optx = 2.4, opty = 0.8, width = 1, pow = 2)
  occ_B_2 <- gaussianShape(seq_env, optx = 2.6, opty = 0.8, width = 1, pow = 2)

  if (run_all) {
    # Simulations
    mat_occA_1 <- sim_occ(occ_A_1, nrep)
    mat_occB_1 <- sim_occ(occ_B_1, nrep)
    mat_occA_2 <- sim_occ(occ_A_2, nrep)
    mat_occB_2 <- sim_occ(occ_B_2, nrep)

    nwd <- length(vc_w_sz)
    pb <- progress_bar$new(format = paste0(cli::symbol$info,
        "computing [:bar] :percent eta: :eta"),
        total = sum(sz - vc_w_sz), clear = FALSE, width = 70)

    ls_res <- list(scenario1 = list(), scenario2 = list())
    # loop over window size
    for (k in seq_len(nwd)) {
      mat_1 <- mat_2 <- matrix(0, nrow = nrep, ncol = sz - vc_w_sz[k])
      for (j in seq_len(ncol(mat_1))) {
        pb$tick()
        rgc <- j - 1 + seq_len(vc_w_sz[k])
        vA1 <- split(mat_occA_1[, rgc], seq_len(nrep))
        vB1 <- split(mat_occB_1[, rgc], seq_len(nrep))
        vA2 <- split(mat_occA_2[, rgc], seq_len(nrep))
        vB2 <- split(mat_occB_2[, rgc], seq_len(nrep))
        mat_1[, j] <- mapply(cooc_sign, vA1, vB1, MoreArgs = list(len = vc_w_sz[k]))
        mat_2[, j] <- mapply(cooc_sign, vA2, vB2, MoreArgs = list(len = vc_w_sz[k]))
      }
      ls_res$scenario1[[k]] <- mat_1
      ls_res$scenario2[[k]] <- mat_2
    }
    res1_f <- lapply(ls_res$scenario1, apply, 2, mean)
    res2_f <- lapply(ls_res$scenario2, apply, 2, mean)

  } else {
    # to be added
    res1_f <- cooccnotinter::arg4_res$scenario1
    res2_f <- cooccnotinter::arg4_res$scenario2
  }

  ############################# PLOT

  # output dir
  output_dir()

  pal2 <- colorRampPalette(pal)(nwd)
  mat <- matrix(c(5, 6, 0, 0, 1, 3, 7, 8, 2, 4, 7, 8), 4)
  yc <- 0.81
  id_env <- seq_env >=0 & seq_env <= 5

  png("output/fig4.png", res = 600, width = 6, height = 6.5, units = "in")

  layout(mat, widths = c(0.18, 1, 1), heights = c(1, 1, 0.14, 0.26))

  par(las = 1, bty = "l", yaxs = "i", mar = c(2.5, 3, 1.5, 1), lend = 1, xaxs = "i")

  ## Scenario 1
  matx <- cbind(c(0, 5), c(-0.02, 1))
  plot(matx, type = "n", ann = FALSE, xlim = c(-.1, 5.1))
  lines(seq_env_d, occ_A_1[id_env], col = "grey10", lwd = 2)
  lines(seq_env_d, occ_B_1[id_env], col = "grey60", lwd = 2)
  text(1.5, yc, labels = expression(P(X[A])), col = "grey10", pos = 3, cex = 1.6)
  text(3.5, yc, labels = expression(P(X[B])), col = "grey60", pos = 3, cex = 1.6)
  mtext("a", 3, at = 0.2, cex = 1.2, line = -1.4)

  ## Scenario 2
  plot(matx, type = "n", ann = FALSE, xlim = c(-.1, 5.1))
  lines(seq_env_d, occ_A_2[id_env], col = "grey10", lwd = 2)
  lines(seq_env_d, occ_B_2[id_env], col = "grey60", lwd = 2)
  text(1.5, yc, labels = expression(P(X[A])), col = "grey10", pos = 3,
    cex = 1.6)
  text(3.5, yc, labels = expression(P(X[B])), col = "grey60", pos = 3,
    cex = 1.6)
  mtext("b", 3, at = 0.2, cex = 1.2, line = -1.4)


  ## Scenario 1
  matx <- cbind(c(0, 5), c(-0.1,.1))
  plot(matx, type = "n", ann = FALSE, xlim = c(-.1, 5.1))
  for (i in seq_len(nwd)) {
    sq <- seq_x(sz, res1_f[[i]])
    lines(seq_env[sq], res1_f[[i]], col = pal2[i])
  }
  abline(h = , lwd = 2)
  mtext("c", 3, at = 0.2, cex = 1.2, line = -1.4)
  abline(h = mean(occ_A_1[id_env] * occ_B_1[id_env]) - mean(occ_A_1[id_env]) * mean(occ_B_1[id_env]), lty = 2)

  ## Scenario 2
  plot(matx, type = "n", ann = FALSE, xlim = c(-.1, 5.1))
  for (i in seq_len(nwd)) {
    sq <- seq_x(sz, res2_f[[i]])
    lines(seq_env[sq], res2_f[[i]], col = pal2[i])
  }
  abline(h = mean(occ_A_2[id_env] * occ_B_2[id_env]) - mean(occ_A_2[id_env]) * mean(occ_B_2[id_env]), lty = 2)
  mtext("d", 3, at = 0.2, cex = 1.2, line = -1.4)

  ##
  par(mar = c(0, 0, 0, 0))
  plot0()
  text(0, 0, srt = 90, labels = "Occurrence probability", cex = 1.6)
  plot0()
  text(0, 0, srt = 90, labels = "Co-occurrence signal", cex = 1.6)

  ##
  plot0()
  text(0, 0, labels = "Environmental gradient", cex = 1.6)

  ##
  par(mar = c(4, 5, 0, 3), mgp = c(1.8, 0.1, 0))
  image(matrix(seq_len(nwd)), col = pal2, ann = FALSE, axes = FALSE)
  axis(1, at = seq(0, 1, length.out = length(vc_w_sz)),
    labels = floor(100*vc_w_sz/length(seq_env_d)) + 1, lwd = 0)
  title(xlab = "Window size (% of the environmental gradient covered)",
    cex.lab = 1.2)

  dev.off()

  success_msg_fig(4)

  invisible(NULL)

}

## Simulate occurrence data
sim_occ <- function(occ_env, nrep) {
  do.call(cbind, lapply(occ_env, function(x) rbinom(nrep, 1, x)))
}

## cooccurrence signal
cooc_sign <- function(x, y, len) {
  sum(x & y)/len - sum(x)/len * sum(y)/len
}

seq_x <- function(n, vec) {
  floor(.5 * (n - length(vec))) + seq_along(vec)
}