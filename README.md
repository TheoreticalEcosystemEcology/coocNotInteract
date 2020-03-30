# coocNotInteract
[![R build status](https://github.com/TheoreticalEcosystemEcology/coocNotInteract/workflows/R-CMD-check/badge.svg)](https://github.com/TheoreticalEcosystemEcology/coocNotInteract/actions)


:book: "Co-occurrence is not evidence of ecological interaction" by F. G. Blanchet, K. Cazelles, D. Gravel - Research compendium. Code to reproduce analysis and figures in <https://www.authorea.com/users/293493/articles/421501-co-occurrence-is-not-evidence-of-ecological-interaction>.


## Install

Our scripts are formatted as an R package to share them easily. As such, use
the package [`remotes`](https://CRAN.R-project.org/package=remotes) to install
this package:

```R
# if remotes is not installed, run
install.packages("remotes")
# then
remotes::install_github("TheoreticalEcosystemEcology/coocNotInteract")
library("coocNotInteract")
```

## Reproduce

For any argument *N* in the manuscript, there is a corresponding function `scr_argN` that runs the analysis and export the corresponding figure.
For instance for argument 1, run

```R
scr_arg1()
```

To execute the entire analysis (for all arguments), use

```R
pipeline()
```
