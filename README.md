# lm.lsa

<!-- badges: start -->

<!-- badges: end -->

lm.lsa permits to estimate linear regressions (based on lm()) using ILSA data from the supported studies: TIMSS, PIRLS, ICILS, ICCS, and PISA. That means, it supports the use of replicate weights and combining results with plausible values.

In addition, it supports the specification of fixed-effects, that is a common approach in econometrician analyses.

Right now the package is in a EXTREMELY early version. Estimates, standard errors, and R-squared should be correct. The other outputs component of summary.lm() are programmed but I have not tested their accuracy.

## What is missing?

This is a very preliminary version. The following aspects are planned to be included in the future, in orden of priority:

* factor(x) and interaction predictors are not accepted, to be implemented. A workaround for now is to include them manually in the dataset.
* no support for additional options (such as subset()), to be implemented.
* only supports linear regression (lm). Probably more typical model functions will be added in a future.




## Installation

You can install the development version of lm.lsa from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("andres5791/lm.lsa")
```

## Example #1: Country-level analyses (country FE)

One common analysis with ILSAs is to create a pseudo-panel of datasets on the country level. Loosely based on Strietholt, Hogrebe & Zachrisson (2020)'s paper, let's analyze the following question: does having pre-schooling have an effect on reading achievement? For that, we have information if the students went to preschool or not and how much time, as reported by parents.

Using base lm(), we would be limited to estimate a model with only one plausible value and without replicate weights. This generates biased standard errors, as they do not calculate correctly the sampling variance and do not include the imputation variance.

``` r
example0 <- lm(ASRREA01 ~ 1 + PRESCH, # formula
                      data=mini_pirls, # example data
                      weights=SENWGT # weight: senate weights
                      )
print(summary(example0))                      
```

We can estimate the above model using lm.lsa(), but using replicate weights and combining the achievement plausible values.

``` r
library(lm.lsa)
example1 <- lm.lsa(ASRREA ~ 1 + PRESCH, # formula
                      data=mini_pirls, # example data
                      wgt="SENWGT", # weight: senate weights
                      rwgts=paste0("RWGT",1:150), # name of replicate weights
                      pvs=list(ASRREA=paste0("ASRREA0",1:5)), # named list with PV variables
                      study="PIRLS" # supports TIMSS, PIRLS, ICILS, ICCS, PISA
                      )
print(example1)                      
```

We estimate country and time fixed-effects, to clear unobserved bias at the country and period level.

``` r
example2 <- lm.lsa(ASRREA ~ 1 + PRESCH, # formula
                      data=mini_pirls, # example data
                      wgt="SENWGT", # weight: senate weights
                      fevar=c("YEAR","IDCNTRY"), # fixed-effects variables
                      rwgts=paste0("RWGT",1:150), # name of replicate weights
                      pvs=list(ASRREA=paste0("ASRREA0",1:5)), # list with PVs variables
                      study="PIRLS"
                      )
print(example2)                      
```

## Example 2: Student fixed-effects

We can also do student fixed-effects and look at the variation across domains. Following the basic idea behind Kennedy, Strietholt, Hogrebe & Strello (2024, working paper https://doi.org/10.35542/osf.io/65fdn), we can look the effect of early activities on literacy and numeracy on reading and mathematics achievement, using TIMSS&PIRLS 2011, taking advantage of the variation between domains and both kind of activities. For this example, we use Finland data.


``` r
mini_tp2011_fin <- mini_pirls[mini_pirls$YEAR %in% "2011" & mini_pirls$IDCNTRY %in% 246,]

# tolong.example() is a function made only for this example
long_tp11_fin <- lm.lsa:::tolong.example(mini_tp2011_fin) 

example3 <- lm.lsa(ACHIEVEMENT ~ 1 + ACTIVITIES, # formula
                      data=long_tp11_fin, # example data
                      wgt="SENWGT", # weight: senate weights
                      fevar=c("IDSTUD"), # fixed-effects variables
                      rwgts=paste0("RWGT",1:150), # name of replicate weights
                      pvs=list(ACHIEVEMENT=paste0("ACHIEVEMENT",1:5)), # list with PVs variables
                      ncores=2, # more cores is more RAM, deactivate parallel
                                # by using '1' core
                      study="PIRLS"
                      )
print(example3)                      
```

