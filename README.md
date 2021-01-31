# penRegmiss

# Large-Scale Survey Data Analysis with Penalized Regression: A Monte Carlo Simulation on Missing Categorical Predictors

Minjeong Rho & Jin Eun Yoo

## Description

The R code reproduces a simulation study in an article to appear in Multivariate Behavioral Research, entitled “Large-Scale Survey Data Analysis with Penalized Regression: A Monte Carlo Simulation on Missing Categorical Predictors.” The code contains data generation, deletion, imputation, and analysis. Specifically, it focuses on penalized regression in the context of incomplete social science large-scale data analysis. 

## Simulation Study

* `penRegmiss.R`  

This R file includes data generating, data deletion and imputation, and running penelized regression. The R functions that we programmed are as follows: 

```R
gen_dt() # data generation, deletion (MAR, MNAR), and imputation (listwise deletion, k-NN, EM)
run_pen() # analysis (group LASSO, group Enet, group Mnet)
```
For more detailed information, please refer to the article in Multivariate Behavioral Research. 

* Note

We recommend you run the code in a high-speed desktop, as it takes more than 10 hours for a single run. 

* Warning messages

You will encounter the following warning messages in obtaining kappa in the condition combinations including listwise deletion. You can simply dismiss them, as they relate to small samples caused by listwise deletion. 

```R
Warning message:
In cohen.kappa1(x, w=w, n.obs=n.obs, alpha=alpha, levels=levels): upper or lower confidence interval exceed abs(1) and set to +- 1. 
```


## Real Data(KCYPS) Study

* `kcyps_pen.RData`

This is data for real data analysis with penalized regression. The original KCYPS dataset is available at https://www.nypi.re.kr/archive/eps.

* `kcyps_rf.RData`

This is data for real data analysis with randomforest. The original KCYPS dataset is available at https://www.nypi.re.kr/archive/eps.

* `KCYPS_code.R`

This R file includes code that applies the penalty regression used in the simulation and randomforest to real data.

## License 

SPDX-FileCopyrightText: © 2021 Minjeong Rho <minjeong019@gmail.com> & Jin Eun Yoo <jineun.yoo@gmail.com>

SPDX-License-Identifier: BSD-3-Clause

## Related article
Yoo, J. E.,& Rho, M. (accepted). Large-scale survey data analysis with penalized regression: A Monte Carlo simulation on missing categorical predictors. Multivariate Behavioral Research. 




