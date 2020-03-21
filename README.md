# coocc_not_inter
[![R build status](https://github.com/TheoreticalEcosystemEcology/coocc_not_inter/workflows/R-CMD-check/badge.svg)](https://github.com/TheoreticalEcosystemEcology/coocc_not_inter/actions)

:book: Blanchet (in rev) DOI:2Badded - Research compendium



Code to reproduce analysis and figure in <https://www.authorea.com/users/293493/articles/421501-co-occurrence-is-not-evidence-of-ecological-interaction>


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
If you want to execute the entire analysis (for all arguments), use

```R
pipeline()
```
