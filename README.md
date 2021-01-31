# penRegmiss

# Large-Scale Survey Data Analysis with Penalized Regression: A Monte Carlo Simulation on Missing Categorical Predictors

Jin Eun Yoo & Minjeong Rho

## Description

The R code reproduces a simulation study in an article to appear in Multivariate Behavioral Research, entitled “Large-Scale Survey Data Analysis with Penalized Regression: A Monte Carlo Simulation on Missing Categorical Predictors.” The code contains data generation, deletion, imputation, and analysis. Specifically, it focuses on penalized regression in the context of incomplete social science large-scale data analysis. The R functions that we programmed are as follows: 

## Simulation Study

* `penRegmiss.R`  

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

* `study2_pen.RData`

* `stud2_rf.RData`

* `KCYPS_code.R`




