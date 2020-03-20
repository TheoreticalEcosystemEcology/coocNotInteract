#' @importFrom crayon blue green
#' @importFrom inSilecoMisc logistic2 gaussianShape scaleWithin
#' @importFrom magrittr %>%
#' @importFrom grDevices dev.off png
#' @importFrom graphics axis box layout lines mtext par points text title
#' @importFrom graphicsutils box2 contrastColors darken envelop plot0 plotImage
#' @keywords internal
NULL


# HELPERS

output_dir <- function(dir = "output") {
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  invisible(dir)
}


## print helper

success_msg <- function(...) {
  message(green(paste0(cli::symbol$tick, " ", ...)))
}

info_msg <- function(...)
  message(blue(paste0(cli::symbol$info, " ", ...)))



## Graph helpers
mlet <- function(let = "a", line = -1)
  mtext(3, text = let, adj = 0.02, line = line)

pal <- c("#071e37", "#297499", "darkorange", "#f4d016")