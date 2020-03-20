# coocc_not_inter
[![R build status](https://github.com/TheoreticalEcosystemEcology/coocc_not_inter/workflows/R-CMD-check/badge.svg)](https://github.com/TheoreticalEcosystemEcology/coocc_not_inter/actions)

:book: Blanchet (in rev) DOI:2Badded - Research compendium

Code to reproduce analysis and figure in...


## Install

```
remotes::install_github("TheoreticalEcosystemEcology/coocc_not_inter")
library("cooccnotinter")
```

## Reproduce

For any argument *N* in the manuscript, there is a corresponding function `scr_argN` that runs the analysis and export the corresponding figure.
For instance for argument 1, run

```R
scr_arg1()
```
If you want to execute all the entire set of argument, use

```R
pipeline()
```
