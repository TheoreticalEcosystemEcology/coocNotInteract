#' @importFrom binom binom.wilson
#' @importFrom crayon blue green
#' @importFrom inSilecoMisc logistic2 gaussianShape scaleWithin
#' @importFrom inSilecoMisc msgInfo msgSuccess
#' @importFrom grDevices colorRampPalette dev.off png rgb
#' @importFrom graphics abline axis box image layout legend lines mtext par
#' @importFrom graphics plot points polygon segments text title
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
    msgInfo("Folder", dir, "created!")
  }
  invisible(dir)
}

msgSuccess_fig <- function(n, dir = "output")
  msgSuccess("Fig", n, "created!", paste0("See ", dir, "/fig", n, ".png"))

## Graph helpers
mlet <- function(let = "a", line = -1)
  mtext(3, text = let, adj = 0.02, line = line)

pal <- c("#021128", "#3e99b5", "darkorange", "#fcdb30")
