#' Calculate probability of each event not happening
#'
#' @param pA probability of event A.
#' @param pB probability of event B.
#' @param covAB covariance between A and B.
#'
#' @export
#' @examples
#' coOccProb(.5, .5, 0) 


coOccProb <- function(pA, pB, covAB){

  stopifnot(pA >= 0 & pA <= 1)
  stopifnot(pB >= 0 & pB <= 1)

  qA <- 1 - pA
  qB <- 1 - pB

  # Calculate variance
  varA <- pA * qA
  varB <- pB * qB

  # Calculate standard deviation
  sdA <- sqrt(varA)
  sdB <- sqrt(varB)

  # Check
  if(covAB > (sdA * sdB)){
    stop(paste("Max covariance is:", (sdA * sdB)))
  }

  # Matrix of probabilities (Teugels 1990)
  mat <- matrix(c(qA * qB, pA * qB, qA * pB, pA * pB,
                      -qB,      qB,     -pB,      pB,
                      -qA,     -pA,      qA,      pA,
                        1,      -1,      -1,      1), nrow = 4, ncol = 4)

  # Vector of covariance to calculate probabilities of each event (Teugels 1990)
  vecCov <- matrix(c(1, 0, 0, covAB),ncol = 1)

  # Vector of probabilities of each event (Teugels 1990)
  prob <- mat %*% vecCov

  rownames(prob) <- c("p00", "p10", "p01", "p11")
  colnames(prob) <- "prob"

  list(prob = prob, cor = covAB/(sdA * sdB))
}
