#' @importFrom crayon blue green
#' @importFrom inSilecoMisc logistic2 gaussianShape scaleWithin
#' @importFrom grDevices colorRampPalette dev.off png
#' @importFrom graphics abline axis box image layout legend lines mtext par
#' @importFrom graphics plot points text title
#' @importFrom graphicsutils box2 contrastColors darken envelop plot0 plotImage
#' @importFrom progress progress_bar
#' @importFrom rootSolve stode
#' @importFrom stats rbinom
#' @keywords internal
NULL


# HELPERS
output_dir <- function(dir = "output") {
  if (!dir.exists(dir)) {
    dir.create(dir)
    info_msg(dir, " created!")
  }
  invisible(dir)
}


## Messages helper
success_msg <- function(...) {
  message(green(paste0(cli::symbol$tick, " ", ...)))
}

success_msg_fig <- function(n, dir = "output")
  success_msg("Fig ", n, " created! See ", dir, "/fig", n, ".png")

info_msg <- function(...) message(blue(paste0(cli::symbol$info, " ", ...)))


## Graph helpers
mlet <- function(let = "a", line = -1)
  mtext(3, text = let, adj = 0.02, line = line)

pal <- c("#021128", "#3e99b5", "darkorange", "#fcdb30")
