# penRegmiss

# Large-Scale Survey Data Analysis with Penalized Regression: A Monte Carlo Simulation on Missing Categorical Predictors

maintainer 
Dr. Minjeong Rho minjeong019@gmail.com

## Simulation (Study I)

### Description

* `penRegmiss.R`  

The R code reproduces a simulation study in an article to appear in Multivariate Behavioral Research, entitled “Large-Scale Survey Data Analysis with Penalized Regression: A Monte Carlo Simulation on Missing Categorical Predictors.” The code contains data generation, deletion, imputation, and analysis. Specifically, it focuses on penalized regression in the context of incomplete social science large-scale data analysis. The R functions that we programmed are as follows: 

```R
gen_dt() # data generation, deletion (MAR, MNAR), and imputation (listwise deletion, k-NN, EM)
run_pen() # analysis (group LASSO, group Enet, group Mnet)
```
For more detailed information, please refer to the article in Multivariate Behavioral Research. 

### Note

We recommend you run the code in a high-speed desktop, as it takes more than 10 hours for a single run. 

### Warning messages

When obtaining kappa, you will encounter the following warning messages in the condition combinations including listwise deletion. The warning messages result from large standard deviations of kappa in the conditions of listwise deletion. You can simply dismiss them; the descriptive statistics of prediction measures including kappa are reported in each condition combination of the Monte Carlo simulation (refer to Table 4 of the related article).

```R
Warning message:
In cohen.kappa1(x, w=w, n.obs=n.obs, alpha=alpha, levels=levels): upper or lower confidence interval exceed abs(1) and set to +- 1. 
```




## Real Data Analysis (Study II) 

### Description

Study II analyzes KCYPS (Korea Children and Youth Panel Study). The original KCYPS dataset can be found at https://www.nypi.re.kr/archive/mps. The R datasets for random forest and penalized regression are kcyps_rf.RData and kcyps_pen.RData. The R code for Study II is KCYPS_code.R. 

* `kcyps_pen.RData`
* `kcyps_rf.RData`
* `KCYPS_code.R`


## License 
SPDX-FileCopyrightText: © 2021 Minjeong Rho <minjeong019@gmail.com> & Jin Eun Yoo <jineun.yoo@gmail.com>

SPDX-License-Identifier: BSD-3-Clause


## Related article #
Yoo, J. E., & Rho, M. (accepted). Large-scale survey data analysis with penalized regression: A Monte Carlo simulation on missing categorical predictors. Multivariate Behavioral Research. 




