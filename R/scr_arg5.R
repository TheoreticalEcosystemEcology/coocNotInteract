#' Argument 5: co-occurrence and sample size
#'
#' Code to reproduce analysis for argument 5 and figure 5.
#'
#' @export


scr_arg5 <- function() {

  # Correlation
  corAB <- c(0.9, 0.5, 0, -0.5, -0.9)

  # Probability of A
  pA <- c(0.1, 0.1, 0.4, 0.8)
  qA <- 1 - pA
  # Probability of B
  pB <- c(0.2, 0.9, 0.6, 0.9)
  qB <- 1 - pB

  # Calculate variance
  varA <- pA * qA
  varB <- pB * qB
  # Calculate standard deviation
  sdA <- sqrt(varA)
  sdB <- sqrt(varB)

  # Covariance to use
  covAB <- corAB  %*% t(sdA * sdB)
  colnames(covAB) <- c("RareRare", "RareVerycommon", "CommonCommon", "VerycommonVerycommon")

  # Result object storing probabilities
  resultProb <- array(dim = c(length(pA), 5, nrow(covAB)))
  dimnames(resultProb)[[1]] <- paste("p1 = ", pA, " - p2 = ", pB, sep = "")
  dimnames(resultProb)[[2]] <- c("p00", "p10", "p01", "p11", "cor")

  # Calculate probabilities (see ?coOccProb)
  for (i in seq_along(pA)){
    for (j in seq_len(nrow(covAB))){
      resBase <- coOccProb(pA[i], pB[i],covAB[j,i])
      resultProb[i, 1:4, j] <- resBase$prob
      resultProb[i, 5, j] <- resBase$cor
    }
  }

  # Result object
  sampleSize <- seq_len(1e5)
  resultCI <- array(dim = c(length(sampleSize), 2, length(pA), nrow(covAB)))
  dimnames(resultCI)[[2]] <- c("lower", "upper")
  dimnames(resultCI)[[3]] <- paste0("p1 = ", pA, " - p2 = ", pB)
  dimnames(resultCI)[[4]] <- paste0("cor = ",corAB)

  # Calculate Wilson's binomial confidence intervals
  for (i in seq_along(pA)) {
    for (j in seq_len(nrow(covAB))) {
      success <- resultProb[i, 4, j] * sampleSize
      try(resultCI[, 1:2, i, j] <-  as.matrix(
        binom.wilson(success,sampleSize)[,5:6]),
        silent = TRUE)
    }
  }

  # Calculate number of samples for different significance level
  #
  # The alpha is calculated using a Sidak correction 1 - (1 - alpha)^(1/2)
  signi.01 <- matrix(NA, nrow = dim(resultCI)[3], ncol = dim(resultCI)[4])
  signi.05 <- matrix(NA, nrow = dim(resultCI)[3], ncol = dim(resultCI)[4])

  for(i in 1:dim(resultCI)[3]){
    for(j in 1:dim(resultCI)[4]){
      signi.01[i,j] <- which(sign(resultCI[,2,i,j] - resultCI[,1,i,j] - (1-(1-0.01)^0.5)) == -1)[1]
      signi.05[i,j] <- which(sign(resultCI[,2,i,j] - resultCI[,1,i,j] - (1-(1-0.05)^0.5)) == -1)[1]

    }
  }


  # output dir
  output_dir()

  # Plot results
  png("output/fig5.png", width = 5, height = 5.5, res = 600, units  = "in")

  par(mar = c(1, 1, .5, .5), oma = c(3, 5, 0, 0))

  couleur <- paste0(colorRampPalette(pal[1:3])(5), "80")

  guideSample <- c(1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000,
      10000, 25000, 50000, 100000)
  guideProb <- seq(0, 1, by = 0.2)

  j = 3
  plot(0.1,0.1, xlim = c(0, 1), ylim = c(1, 100000),
       type = "n", log = "y",
       xaxt = "n", yaxt = "n", las = "1",
       xlab = "", ylab = "")

  abline(h = guideSample, col = "lightgrey")
  abline(v = guideProb, col = "lightgrey")
  axis(2, at = guideSample, las = 2, cex.axis = 1.1, mgp = c(3, .8, 0))
  axis(1, at = guideProb, las = 1, cex.axis = 1.1, padj = 0.5,
      mgp = c(3, .4, 0))

  sqy <- seq_len(1e5)
  for(i in seq_len(dim(resultCI)[4])){
    polygon(c(resultCI[, 1, j, i], resultCI[rev(sqy), 2, j, i]),
            c(sampleSize, sampleSize[rev(sqy)]),
            border = FALSE,
            col = couleur[i])
  }

  for(i in seq_len(dim(resultCI)[4])){
    segments(y0 = signi.05[j, i],
             y1 = signi.05[j, i],
             x0 = resultCI[signi.05[j, i],1 ,j ,i],
             x1 = resultCI[signi.05[j, i],2 ,j ,i], lwd = 1.4)
  }

  segments(x0 = resultProb[3, 4, ],
           x1 = resultProb[3, 4, ],
           y0 = rep(0.1, 5),
           y1= signi.05[j, ], lwd = 1.4)

  legend("topright", legend = c(0.9, 0.5, 0.0, -0.5, -0.9), cex = 1.25,
         box.lwd = .8, pch = 22, pt.bg = couleur, col = NA, pt.cex = 2.4,
         title = "Correlation")

  mtext("Probability of co-occurrence", side = 1, line = 1.4,
        outer = TRUE, cex = 1.4)
  mtext("Sample size", side = 2, line = 3, outer = TRUE, cex = 1.4)

  dev.off()

  msgSuccess_fig(5)

  invisible(NULL)

}
