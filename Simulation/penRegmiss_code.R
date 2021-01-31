# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::: Large-Scale Survey Data Analysis with Penalized Regression :::::::::::::::::::
# :::::::::::::: : A Monte Carlo Simulation on Missing Categorical Predictors :::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Jin Eun Yoo, Minjeong Rho
# Simulation Codes

# :: load package
library(simstudy)

# ::::::: Distributions and parameters for large-scale data generation :::::::

bformular <- c(rep(0.5, 10), rep(0.2, 10), rep(0.05, 10)) # coefficients for Bernoulli variables 
parcel_num <- rep(10, 26) # 26 item parcels of 10 items each
parcel_min <- c(rep(0.05, 13), rep(0.2, 13)) # minimum values of variance-covariance matrix elements

# coefficients of logistic regression
cf1 <- rep(c(.3, -.3, -.5, .5), each = 3) 
cf2 <- c(.2, -.4, .3, -.6, -.3, rep(.3, 2), rep(.2, 3))
cff_y <- c(cf1, cf2)

pen <- c("grLasso", "grLasso", "grMCP") # 3 penalized regression methods
aph <- c(1, 0.5, 0.5)  #alpha for lasso, enet, and mnet, respectively
split.rate <- 0.7      #splitting data into train & test with the ratio of 7:3

# ld:listwise deletion, knn: k-nn imputation, em: EM imputation
method <- c("ld", "knn", "em") 

rst1 <- list(); rst2 <- list()

rst_mar <- list(); rst_mnar <- list()

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::    gen_dt: function for data generation    :::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::

gen_dt <- function(seednum) {
  set.seed(seednum * 23456)

  # generating 100 clusters (schools), 200 observations (students) each
  gen_school <-
    defData(
      varname = "s0",
      dist = "normal",
      formula = 0,
      variance = 1,
      id = "idSchool"
    )
  gen_school <-
    defData(gen_school,
      varname = "nStudents",
      dist = "nonrandom",  # the same number of students per school
      formula = 200
    )
  dt_school <- genData(100, gen_school)

  # generating Bernoulli variables
  gen_student <-
    defData(
      varname = "B1",
      dist = "binary",
      formula = bformular[1],
      id = "id"
    )
  for (i in 2:30) {
    gen_student <-
      defDataAdd(
        gen_student,
        varname = paste("B", i, sep = ""),
        formula = bformular[i],
        dist = "binary"
      )
  }
  # generating Poisson variables
  for (i in 1:5) {
    gen_student <-
      defDataAdd(
        gen_student,
        varname = paste("P", i, sep = ""),
        formula = 3,
        dist = "poisson"
      )
  }
  # generating categorical (multinomial) variables
  for (i in 1:5) {
    gen_student <-
      defDataAdd(
        gen_student,
        varname = paste("M", i, sep = ""),
        formula = "0.4;0.3;0.2;0.1",
        dist = "categorical"
      )
  }

  dt_student <-
    genCluster(
      dt_school,
      cLevelVar = "idSchool",
      numIndsVar = "nStudents",
      level1ID = "idChild"
    )

  # generating Likert-scaled variables in item parcels
  for (i in 1:26) {
    # variance-covariance matrix generation for 26 item parcels
    corm <-
      matrix(
        runif(parcel_num[i]^2, min = parcel_min[i], max = 0.7),
        nrow = parcel_num[i],
        ncol = parcel_num[i]
      )
    corm[upper.tri(corm)] <- t(corm)[upper.tri(corm)]
    diag(corm) <- runif(parcel_num[i], min = 0.6, max = 0.95) # values of variance-covariance matrix
    corm <- corpcor::make.positive.definite(corm, tol = 1e-3) # to make positive definite matrix
    diag.s <- runif(parcel_num[i], min = 0.6, max = 0.95) 
    # diagonal elements of variance covariance matrix between .6 and .95
    m <- runif(parcel_num[i], min = 2.6, max = 3.2) # means between 2.6 and 3.2 
    
    # correlated data generation
    ii <- (i - 1) * 10 + 1
    dt_student <- addCorData(
      dt_student,
      idname = "idChild",
      mu = m,
      sigma = diag.s,
      corMatrix = corm,
      cnames = paste("L", c(ii:c(ii + 9)), sep = "")
    )
  }

  # generating data of 20,000 students and 304 variables (including IDs)
  dt_student <-
    addColumns(gen_student, dt_student) 

  # sampling 20 students from each of the 200 schools
  set.seed(seednum * 23456)
  st_num <- seq(1, 20000, by = 200)
  smp_idx <- c()

  for (i in seq_along(st_num)) {
    ff <- sample(st_num[i]:(st_num[i] + 199), 20, replace = F)
    smp_idx <- c(smp_idx, ff)
  }
  
  dt1 <- data.frame(dt_student[smp_idx, ])
  schid <- dt1[, 1] # extracting school ID
  dt1 <-
    dt1[, c(5:length(dt1))] # dt1[1:4]: other IDs not used in subsequent analyses
  dt1[, 1:260] <- round(dt1[, 1:260], 0) # rounding Likert-scaled variables 

    # managing Likert-scaled variables in 1-5 range
  for (i in 1:260) {
    dt1[, i] <- car::recode(dt1[, i], "-1=1; 0=1; 6=5; 7=5")
  }

  # generating dummy coding for 5 categorical variables
  ss <- c(); s1 <- c()

  grp_vars <- colnames(dt1)[296:300] # categorical variables
  for (i in 1:5) {
    s1 <- dt1[, grp_vars[i]]
    # 3 dummy variables created for each variable of 4 categories
    for (j in 1:3) {
      s1 <- ifelse(dt1[, grp_vars[i]] == j, 1, 0)
      ss <- cbind(ss, s1)
      colnames(ss)[dim(ss)[2]] <- paste(grp_vars[i], j, sep = ".")
    }
  }
  
  # variables combined in forydt dataframe
  forydt <- data.frame(dt1[, 1:295], ss)  

  # true nonzero Xs (a total of 20) 
  var_x <- forydt[, c(
    1:3, 35, 36, 39, 101:103, 205:207, 151, 161, 171, 181, 265,
    270, 290, 308:310
  )]
  
  abx <- cff_y %*% t(var_x) # logit
  prob <- exp(abx) / (1 + exp(abx)) # probability
  var_y <- ifelse(prob >= 0.5, 1, 0) # cut-off of .5
  var_y <- as.numeric(var_y) # response variable (y)
  
  cmplt_dt <- data.frame(dt1, var_y) # final, complete data
  
 
  ############# missing data generation #############
  # MAR #
  # Cause of missingness variables: L10, L20, L30, L40
  mss_gen1 <- rep(c(colnames(dt1)[c(10, 20, 30, 40)]), each = 15) # 60 variables for 1% missing rate 
  mss_gen2 <- rep(c(colnames(dt1)[c(10, 20, 30, 40)]), each = 5) #20 variables for 10% missing rate
  mss_var <-
    sample(colnames(dt1)[-c(10, 20, 30, 40)], 80, replace = F)

  mar_dt <-
    # 1% missing for 60 variables
    missMethods::delete_MAR_1_to_x(
      dt1,
      p = 0.01,
      x = 2,
      cols_mis = mss_var[1:60], 
      cols_ctrl = mss_gen1
    )
  mar_dt <-
    # 10% missing for 20 variables
    missMethods::delete_MAR_1_to_x(
      mar_dt,
      p = 0.1,
      x = 2,
      cols_mis = mss_var[61:80],
      cols_ctrl = mss_gen2
    )

  sch_mss <- t(missMethods::delete_MCAR(t(mar_dt[1801:2000, ]), 0.2))
  # Students in the last 5 schools had 20% of the data points deleted in MCAR.

  mar_dt2 <- rbind(mar_dt[1:1800, ], sch_mss) # MAR data
  mnar_dt <- mar_dt2[, -c(10, 20, 30, 40)] # MNAR data (excluding cause of missingness variables)

  ####### listwise, k-nn, and em  for MAR data ########
  mar_dtt <- data.frame(mar_dt2, var_y) # MAR data
  
  # listwise deletion
    ma_ld <- na.omit(mar_dtt)

  # k-NN
  mar_k_dt <- data.frame(mar_dt2, var_y) 
  for (i in 296:300) {
    mar_k_dt[, i] <- as.factor(mar_k_dt[, i])
  }  # categorical variables as factors
  ma_kn <-
    VIM::kNN(mar_k_dt, k = sqrt(nrow(na.omit(mar_k_dt))), imp_var = F)

  # em imputation
  mar_e_dt <- data.frame(schid, mar_dt2, var_y)
  ordvars <- colnames(mar_dt)[c(1:295)] # Likert, Poisson, and Bernoulli variables  
  grp_vars <- colnames(mar_dt)[296:300] # categorical variables
  
  em1 <-
    Amelia::amelia(
      mar_e_dt,
      m = 1,
      boot.type = "none",
      ords = ordvars,
      noms = grp_vars,
      cs = "schid"
    )
  ma_em <- em1$imputations$imp1[, -1] # the 1st imputed dataset

  d_mar <- list(ma_ld, ma_kn, ma_em, cmplt_dt)

  # dat1: data for penalized regression
  mar_l <- list()
  for (dd in 1:4) {
    dat1 <- d_mar[[dd]]
    ss <- c()
    s1 <- c()

    for (i in seq_along(dat1)) {
      dat1[, i] <- as.integer(dat1[, i]) # factors back to integers
    }

    # dummy coding categorical variables
    for (i in 1:5) {
      s1 <- dat1[, grp_vars[i]]
      for (j in 1:3) {
        s1 <- ifelse(dat1[, grp_vars[i]] == j, 1, 0)
        ss <- cbind(ss, s1)
        colnames(ss)[dim(ss)[2]] <- paste(grp_vars[i], j, sep = ".")
      }
    }

    # dat2: data ready for analysis 
    dat2 <- data.frame(dat1[, c(1:295, 301)], ss)
    
    # g_idx: variable index
    g_idx <- c(1:295, rep(c(296:300), each = 3))  
    var_y <- dat2$var_y # response variable 
    dat2$var_y <- NULL

    mar_l[[dd]] <- list(
      "dat" = dat2,
      "var_y" = var_y,
      "g_idx" = g_idx
    )
  }

  ####### listwise, k-nn, and em for MNAR data ########
  mnar_dtt <- data.frame(mnar_dt, var_y)
  
  # listwise deletion
  mn_ld <- na.omit(mnar_dtt) 

  # k-nn
  mn_k_dt <- data.frame(mnar_dt, var_y)
  for (i in 292:296) {
    mn_k_dt[, i] <- as.factor(mn_k_dt[, i])   # categorical variables as factors
  }
  mn_kn <-
    VIM::kNN(mn_k_dt, k = sqrt(nrow(na.omit(mn_k_dt))), imp_var = F)

  # em imputation
  mn_e_dt <- data.frame(schid, mnar_dt, var_y)
  ordvars <- colnames(mnar_dt)[c(1:291)] # Likert, Poisson, and Bernoulli variables
  grp_vars <- colnames(mar_dt)[292:296] # categorical variables #? I've changed the numbers to 292 and 296. 
  em1 <-
    Amelia::amelia(
      mn_e_dt,
      m = 1,
      boot.type = "none",
      ords = ordvars,
      noms = grp_vars,
      cs = "schid"
    )
  mn_em <- em1$imputations$imp1[, -1] # the 1st imputed dataset

  d_mnar <- list(mn_ld, mn_kn, mn_em)

  #dat1: data for penalized regression
  mnar_l <- list()
  for (dd in 1:3) {
    dat1 <- d_mnar[[dd]]
    ss <- c()
    s1 <- c()

    for (i in seq_along(dat1)) {
      dat1[, i] <- as.integer(dat1[, i]) # factors back to integers
    }

    # dummy coding categorical variables
    for (i in 1:5) {
      s1 <- dat1[, grp_vars[i]]
      for (j in 1:3) {
        s1 <- ifelse(dat1[, grp_vars[i]] == j, 1, 0)
        ss <- cbind(ss, s1)
        colnames(ss)[dim(ss)[2]] <- paste(grp_vars[i], j, sep = ".")
      }
    }

    # dat2: data ready for analysis 
    dat2 <- data.frame(dat1[, c(1:291, 297)], ss)

    # g_idx: variable index
    g_idx <- c(1:291, rep(c(292:296), each = 3))
    var_y <- dat2$var_y
    dat2$var_y <- NULL

    mnar_l[[dd]] <- list(
      "dat" = dat2,
      "var_y" = var_y,
      "g_idx" = g_idx
    )
  }

  return(
    list(
      "mar_l" = mar_l,
      "mnar_l" = mnar_l
    )
  )
}

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::: run_pen: function to run penalized regression :::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

run_pen <- function() {
  for (k in 1:4) {
    # 4 datasets: listwise, k-nn, em, complete data
    
    # MAR
    x_dt <- as.matrix(mar_l[[k]][[1]])
    y_dt <- as.double(mar_l[[k]][[2]])
    g_idx <- mar_l[[k]][[3]]

    for (h in 1:3) {
    # 3 penalized regression methods: lasso, enet, mnet
        for (j in 1:100) {
          
        # 100 iterations
        set.seed(h * j * 12345)

        # data split into training and test
        tr_idx <- c(
          sample(c(which(y_dt == 0)), table(y_dt)[1] * split.rate),
          sample(c(which(y_dt == 1)), table(y_dt)[2] * split.rate)
        )
        x <- as.matrix(x_dt[tr_idx, ])
        xx <- as.matrix(x_dt[-tr_idx, ])
        y <- as.double(y_dt)[tr_idx]
        yy <- as.double(y_dt)[-tr_idx]

        #10-fold CV
        cv_rst <-
          grpreg::cv.grpreg(
            X = x,
            y = y,
            family = "binomial",
            group = g_idx,
            penalty = pen[h],
            alpha = aph[h]
          )
        
        # to obtain nonzero coefficients
        sltvar <-
          colnames(x_dt)[which(coef(cv_rst, s = cv_rst$lambda.min) != 0) - 1]
        cff <- coef(cv_rst, s = cv_rst$lambda.min)[-1]
        
        # to obtain prediction measures: accuracy, kappa, and AUC
        pred_y <-
          predict(cv_rst, xx, type = "class", s = cv_rst$lambda.min)
        
        perff <- table(yy, pred_y)
        if (dim(perff)[2] == 1) {
          kpp <- NA
          acc <- NA
        } else {
          kpp <- psych::cohen.kappa(perff)$kappa   # kappa
          acc <- (perff[1, 1] + perff[2, 2]) / sum(perff)  #accuracy
        }
        au <- pROC::auc(yy, pred_y, direction="<", levels=c(0,1)) # AUC

        rst1[[j]] <- list(acc, au, kpp, sltvar, cff)
      }
      rst2[[h]] <- rst1
    }
    rst_mar[[k]] <- rst2
  }

  # MNAR
  for (k in 1:3) {
    x_dt <- as.matrix(mnar_l[[k]][[1]])
    y_dt <- as.double(mnar_l[[k]][[2]])
    g_idx <- mnar_l[[k]][[3]]

    for (h in 1:3) {
    # 3 penalized regression methods: lasso, enet, mnet
      for (j in 1:100) {
        
        # 100 iterations
        set.seed(h * j * 12345)
        
        # data split into training and test
        tr_idx <-
          c(
            sample(c(which(y_dt == 0)), table(y_dt)[1] * split.rate),
            sample(c(which(y_dt == 1)), table(y_dt)[2] * split.rate)
          )
        x <- as.matrix(x_dt[tr_idx, ])
        xx <- as.matrix(x_dt[-tr_idx, ])
        y <- as.double(y_dt)[tr_idx]
        yy <- as.double(y_dt)[-tr_idx]

        #10-fold CV
        cv_rst <-
          grpreg::cv.grpreg(
            X = x,
            y = y,
            family = "binomial",
            group = g_idx,
            penalty = pen[h],
            alpha = aph[h]
          )
        
        # to obtain nonzero coefficients
        sltvar <-
          colnames(x_dt)[which(coef(cv_rst, s = cv_rst$lambda.min) != 0) - 1]
        cff <- coef(cv_rst, s = cv_rst$lambda.min)[-1]
        
        # to obtain prediction measures: accuracy, kappa, and AUC
        pred_y <-
          predict(cv_rst, xx, type = "class", s = cv_rst$lambda.min)
        
        perff <- table(yy, pred_y)
        if (dim(perff)[2] == 1) {
          kpp <- NA
          acc <- NA
        } else {
          kpp <- psych::cohen.kappa(perff)$kappa # kappa
          acc <- (perff[1, 1] + perff[2, 2]) / sum(perff) # accuracy
        }
        au <- pROC::auc(yy, pred_y, direction="<", levels=c(0,1)) # AUC

        rst1[[j]] <- list(acc, au, kpp, sltvar, cff)
      }
      rst2[[h]] <- rst1
    }
    rst_mnar[[k]] <- rst2
  }

  return(list("rst_mar" = rst_mar, "rst_mnar" = rst_mnar))
}


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  
# :::::::: executing simulation and saving the results in RData ::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  

#n_iter: iteration number
n_iter <- 100
for (seednum in 1:n_iter) {
  impu <- gen_dt(seednum)
  save(impu, file = paste("impu", seednum, "RData", sep = "."))
  mar_l <- impu[[1]]
  mnar_l <- impu[[2]]

  rst <- run_pen()
  save(rst, file = paste("rst.iter", seednum, "RData", sep = "."))

  print(paste(seednum, Sys.time(), sep = "##"))
}
