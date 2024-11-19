# lm.lsa

<!-- badges: start -->

<!-- badges: end -->

lm.lsa permits to estimate linear regressions (based on lm()) using ILSA data from the supported studies: TIMSS, PIRLS, ICILS, ICCS, and PISA. That means, it supports the use of replicate weights and combining results with plausible values.

In addition, it supports the specification of fixed-effects, that is a common approach in econometrician analyses.

Right now the package is in a EXTREMELY early version. Estimates, standard errors, and R-squared should be correct. The other outputs component of summary.lm() are programmed but I have not tested their accuracy.

## Installation

You can install the development version of lm.lsa from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("andres5791/lm.lsa")
```

## What is missing?

Right now it only supports linear regression. Probably more typical model functions will be added in a future.

The functions do not support right now additional options (such as subset()), to be implemented.

Right now factor(x) predictors are not accepted, to be implemented.

## Example #1: Country-level analyses (country FE)

One common analysis with ILSAs is to create a pseudo-panel of datasets on the country level. Loosely based on Strietholt, Hogrebe & Zachrisson (2020)'s paper, let's analyze the following question: does having pre-schooling have an effect on reading achievement? For that, we have information if the students went to preschool or not and how much time, as reported by parents.

``` r
example0 <- lm(ASRREA01 ~ 1 + PRESCH, # formula
                      data=mini_pirls, # example data
                      weights="SENWGT" # weight: senate weights
                      )
print(summary(example0))                      
```





We estimate country and time fixed-effects, to clear unobserved bias at the country and period level.

``` r
library(lm.lsa)
library(parallel,dplyr)
example1 <- lm.pv.rep(ASRREA ~ 1 + PRESCH, # formula
                      data=mini_pirls, # example data
                      wgt="SENWGT", # weight: senate weights
                      fevar=c("YEAR","IDCNTRY"), # fixed-effects variables
                      rwgts=paste0("RWGT",1:150), # name of replicate weights
                      pvs=list(ASRREA=paste0("ASRREA0",1:5)), # list with PVs variables
                      ncores=2, # more cores is more RAM, deactivate parallel
                                # by using '1' core
                      study="PIRLS"
                      )
print(example1)                      
```
