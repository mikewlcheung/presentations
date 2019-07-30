---
title: "Handling Missing Covariates in Mixed-Effects Meta-Analysis with Full-Information Maximum Likelihood"
author: "Mike W.-L. Cheung"
date: 'July 30, 2019'
output:
  html_document:
    keep_md: yes
    self_contained: yes
    theme: united
    toc: yes
editor_options: 
  chunk_output_type: console
---

# MCAR

```r
library(metaSEM)
```

```
## Loading required package: OpenMx
```

```
## To take full advantage of multiple cores, use:
##   mxOption(key='Number of Threads', value=parallel::detectCores()) #now
##   Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) #before library(OpenMx)
```

```
## "SLSQP" is set as the default optimizer in OpenMx.
```

```
## mxOption(NULL, "Gradient algorithm") is set at "central".
```

```
## mxOption(NULL, "Optimality tolerance") is set at "6.3e-14".
```

```
## mxOption(NULL, "Gradient iterations") is set at "2".
```

```r
## Select part of the data
Tenenbaum02 <- Tenenbaum02[, c("r", "v", "Offspring_age", "Year_pub")]

## Set seed for reproducibility
set.seed(1234567)

## Set 40% of covariate as missing
missing_per <- 0.4

## MCAR
index <- round(nrow(Tenenbaum02)*missing_per)
index <- rep(c(TRUE, FALSE), times=c(index, nrow(Tenenbaum02)-index))
index <- sample(index)
my.MCAR <- Tenenbaum02
my.MCAR[index, "Offspring_age"] <- NA
## Center the covariate
my.MCAR$Offspring_age <- scale(my.MCAR$Offspring_age, scale=FALSE)
my.MCAR
```

```
##        r            v Offspring_age Year_pub
## 1   0.12 0.0029084053            NA     1983
## 2  -0.08 0.0099721309   -48.3448276     1991
## 3  -0.05 0.0103646484   -72.3448276     1980
## 4  -0.08 0.0094022949   -69.3448276     1979
## 5   0.15 0.0013089127   209.6551724     1986
## 6   0.12 0.0404753067   -60.3448276     1988
## 7   0.17 0.0052101393            NA     1988
## 8   0.34 0.0181898456   107.6551724     1984
## 9  -0.01 0.0020238867            NA     1993
## 10  0.33 0.0048124801            NA     1998
## 11  0.40 0.0147000000   -93.3448276     1989
## 12  0.30 0.0138016667   -90.3448276     1992
## 13  0.07 0.0162331805   -57.3448276     2001
## 14 -0.02 0.0099920016            NA     1985
## 15  0.19 0.0092910321    -5.3448276     1985
## 16  0.15 0.0006964331    23.6551724     1999
## 17  0.19 0.0032148900   -18.3448276     1995
## 18  0.02 0.0057097152   -30.3448276     1980
## 19  0.47 0.0085492508            NA     1981
## 20  0.19 0.0046455161     5.6551724     1999
## 21  0.33 0.0124071752            NA     1980
## 22  0.06 0.0157589359   -36.3448276     1980
## 23  0.05 0.0033166875            NA     1990
## 24  0.24 0.0024877248            NA     1993
## 25  0.14 0.0123228738            NA     1977
## 26 -0.02 0.0285485760            NA     1993
## 27  0.28 0.0066877682            NA     1999
## 28  0.14 0.0034825513    35.6551724     1997
## 29  0.27 0.0245575546   -42.3448276     1984
## 30  0.38 0.0140779108   -12.3448276     1977
## 31  0.52 0.0212926464            NA     1982
## 32  0.66 0.0127418944    23.6551724     1982
## 33  0.36 0.0303038464    23.6551724     1982
## 34  0.21 0.0042698356    11.6551724     1998
## 35  0.19 0.0042231964    11.6551724     1998
## 36  0.14 0.0184843108            NA     1975
## 37  0.36 0.0102377859    95.6551724     1980
## 38  0.09 0.0006314927    23.6551724     1999
## 39  0.16 0.0053946327            NA     2001
## 40 -0.07 0.0319427100            NA     1987
## 41  0.36 0.0140295585    89.6551724     1979
## 42  0.36 0.0008270700    95.6551724     1983
## 43  0.22 0.0238300674    -0.3448276     1996
## 44  0.04 0.0066899501   -72.3448276     1995
## 45  0.06 0.0146001906            NA     1979
## 46  0.41 0.0150447307            NA     1984
## 47  0.04 0.0140394727            NA     1984
## 48  0.23 0.0067954425   -48.3448276     1990
```

```r
## y: effect size
## v: sampling variance
## x: covariate
fit.MCAR <- metaFIML(y=r, v=v, x=Offspring_age, data=my.MCAR)
summary(fit.MCAR)
```

```
## 
## Call:
## metaFIML(y = r, v = v, x = Offspring_age, data = my.MCAR)
## 
## 95% confidence intervals: z statistic approximation (robust=FALSE)
## Coefficients:
##               Estimate   Std.Error      lbound      ubound z value
## Tau2_1_1    1.2100e-02  4.5139e-03  3.2526e-03  2.0947e-02  2.6805
## CovX1_X1    4.6234e+03  1.2127e+03  2.2464e+03  7.0003e+03  3.8123
## Slope1_1    6.8415e-04  3.5723e-04 -1.6012e-05  1.3843e-03  1.9151
## Intercept1  1.8181e-01  2.1710e-02  1.3926e-01  2.2436e-01  8.3748
## MeanX1     -3.8490e-01  1.2377e+01 -2.4644e+01  2.3874e+01 -0.0311
##             Pr(>|z|)    
## Tau2_1_1   0.0073507 ** 
## CovX1_X1   0.0001377 ***
## Slope1_1   0.0554744 .  
## Intercept1 < 2.2e-16 ***
## MeanX1     0.9751917    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Q statistic on the homogeneity of effect sizes: 179.0075
## Degrees of freedom of the Q statistic: 47
## P value of the Q statistic: 0
## 
## Explained variances (R2):
##                            y1
## Tau2 (no predictor)    0.0142
## Tau2 (with predictors) 0.0121
## R2                     0.1493
## 
## Number of studies (or clusters): 48
## Number of observed statistics: 77
## Number of estimated parameters: 5
## Degrees of freedom: 72
## -2 log likelihood: 284.1819 
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values may indicate problems.)
```

# MAR

```r
## MAR depending on the Year of Publication
index <- Tenenbaum02$Year_pub <= quantile(Tenenbaum02$Year_pub, missing_per)
my.MAR <- Tenenbaum02
my.MAR[index, "Offspring_age"] <- NA
my.MAR$Offspring_age <- scale(my.MAR$Offspring_age, scale=FALSE)
my.MAR$Year_pub <- scale(my.MAR$Year_pub, scale=FALSE)
my.MAR
```

```
##        r            v Offspring_age     Year_pub
## 1   0.12 0.0029084053            NA  -4.91666667
## 2  -0.08 0.0099721309   -43.8888889   3.08333333
## 3  -0.05 0.0103646484            NA  -7.91666667
## 4  -0.08 0.0094022949            NA  -8.91666667
## 5   0.15 0.0013089127   214.1111111  -1.91666667
## 6   0.12 0.0404753067   -55.8888889   0.08333333
## 7   0.17 0.0052101393    40.1111111   0.08333333
## 8   0.34 0.0181898456            NA  -3.91666667
## 9  -0.01 0.0020238867   -19.8888889   5.08333333
## 10  0.33 0.0048124801   112.1111111  10.08333333
## 11  0.40 0.0147000000   -88.8888889   1.08333333
## 12  0.30 0.0138016667   -85.8888889   4.08333333
## 13  0.07 0.0162331805   -52.8888889  13.08333333
## 14 -0.02 0.0099920016   -34.8888889  -2.91666667
## 15  0.19 0.0092910321    -0.8888889  -2.91666667
## 16  0.15 0.0006964331    28.1111111  11.08333333
## 17  0.19 0.0032148900   -13.8888889   7.08333333
## 18  0.02 0.0057097152            NA  -7.91666667
## 19  0.47 0.0085492508            NA  -6.91666667
## 20  0.19 0.0046455161    10.1111111  11.08333333
## 21  0.33 0.0124071752            NA  -7.91666667
## 22  0.06 0.0157589359            NA  -7.91666667
## 23  0.05 0.0033166875    22.1111111   2.08333333
## 24  0.24 0.0024877248   -19.8888889   5.08333333
## 25  0.14 0.0123228738            NA -10.91666667
## 26 -0.02 0.0285485760   -50.8888889   5.08333333
## 27  0.28 0.0066877682   148.1111111  11.08333333
## 28  0.14 0.0034825513    40.1111111   9.08333333
## 29  0.27 0.0245575546            NA  -3.91666667
## 30  0.38 0.0140779108            NA -10.91666667
## 31  0.52 0.0212926464            NA  -5.91666667
## 32  0.66 0.0127418944            NA  -5.91666667
## 33  0.36 0.0303038464            NA  -5.91666667
## 34  0.21 0.0042698356    16.1111111  10.08333333
## 35  0.19 0.0042231964    16.1111111  10.08333333
## 36  0.14 0.0184843108            NA -12.91666667
## 37  0.36 0.0102377859            NA  -7.91666667
## 38  0.09 0.0006314927    28.1111111  11.08333333
## 39  0.16 0.0053946327   -31.8888889  13.08333333
## 40 -0.07 0.0319427100   -67.8888889  -0.91666667
## 41  0.36 0.0140295585            NA  -8.91666667
## 42  0.36 0.0008270700            NA  -4.91666667
## 43  0.22 0.0238300674     4.1111111   8.08333333
## 44  0.04 0.0066899501   -67.8888889   7.08333333
## 45  0.06 0.0146001906            NA  -8.91666667
## 46  0.41 0.0150447307            NA  -3.91666667
## 47  0.04 0.0140394727            NA  -3.91666667
## 48  0.23 0.0067954425   -43.8888889   2.08333333
```

```r
## av: auxiliary variable
fit.MAR <- metaFIML(y=r, v=v, x=Offspring_age, av=Year_pub, data=my.MAR)
summary(fit.MAR)
```

```
## 
## Call:
## metaFIML(y = r, v = v, x = Offspring_age, av = Year_pub, data = my.MAR)
## 
## 95% confidence intervals: z statistic approximation (robust=FALSE)
## Coefficients:
##               Estimate   Std.Error      lbound      ubound z value
## Tau2_1_1    1.3683e-02  4.9418e-03  3.9969e-03  2.3368e-02  2.7688
## CovX1_X1    4.7635e+03  1.3254e+03  2.1658e+03  7.3612e+03  3.5940
## CovX2_X1    5.9504e+01  1.5896e+02 -2.5206e+02  3.7107e+02  0.3743
## CovX2_X2    5.7118e+01  1.1659e+01  3.4266e+01  7.9970e+01  4.8990
## CovX2_Y1   -1.9850e-01  1.6875e-01 -5.2925e-01  1.3225e-01 -1.1763
## Slope1_1    3.9885e-04  5.4577e-04 -6.7083e-04  1.4685e-03  0.7308
## Intercept1  1.8709e-01  2.3250e-02  1.4152e-01  2.3266e-01  8.0469
## MeanX1     -3.6621e+00  2.1194e+01 -4.5201e+01  3.7877e+01 -0.1728
## MeanX2     -2.8052e-07  1.0909e+00 -2.1382e+00  2.1382e+00  0.0000
##             Pr(>|z|)    
## Tau2_1_1   0.0056271 ** 
## CovX1_X1   0.0003256 ***
## CovX2_X1   0.7081627    
## CovX2_X2   9.634e-07 ***
## CovX2_Y1   0.2394900    
## Slope1_1   0.4648969    
## Intercept1 8.882e-16 ***
## MeanX1     0.8628131    
## MeanX2     0.9999998    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Q statistic on the homogeneity of effect sizes: 179.0075
## Degrees of freedom of the Q statistic: 47
## P value of the Q statistic: 0
## 
## Explained variances (R2):
##                            y1
## Tau2 (no predictor)    0.0143
## Tau2 (with predictors) 0.0137
## R2                     0.0404
## 
## Number of studies (or clusters): 48
## Number of observed statistics: 123
## Number of estimated parameters: 9
## Degrees of freedom: 114
## -2 log likelihood: 593.2959 
## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
## Other values may indicate problems.)
```

```r
sessionInfo()
```

```
## R version 3.6.1 (2019-07-05)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Linux Mint 19.1
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/openblas/libblas.so.3
## LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.2.20.so
## 
## locale:
##  [1] LC_CTYPE=en_SG.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_SG.UTF-8        LC_COLLATE=en_SG.UTF-8    
##  [5] LC_MONETARY=en_SG.UTF-8    LC_MESSAGES=en_SG.UTF-8   
##  [7] LC_PAPER=en_SG.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_SG.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] metaSEM_1.2.2.1 OpenMx_2.13.2  
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.2      mvtnorm_1.0-11  lattice_0.20-38 digest_0.6.20  
##  [5] MASS_7.3-51.4   grid_3.6.1      stats4_3.6.1    magrittr_1.5   
##  [9] ellipse_0.4.1   evaluate_0.14   stringi_1.4.3   Matrix_1.2-17  
## [13] pbivnorm_0.6.0  rmarkdown_1.14  tools_3.6.1     stringr_1.4.0  
## [17] xfun_0.8        yaml_2.2.0      parallel_3.6.1  compiler_3.6.1 
## [21] mnormt_1.5-5    htmltools_0.3.6 lavaan_0.6-4    knitr_1.23
```
