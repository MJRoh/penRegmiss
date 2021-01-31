# real data(KCYPS) analysis

########### Penalized regression ###########
# load data
load("kcyps_pen.RData")
# dtlist[[1]]: ld data, dtlist[[2]]: knn data, dtlist[[3]]: em data

pen <- c("grLasso", "grLasso", "grMCP")
mss <- c("ld", "knn", "em")
aph <- c(1, 0.5, 0.5)
# alpha values for group LASSO, group Enet, and group Mnet, respectively

auk <- c()
sltvars <- list()
rst1 <- list(); rst2 <- list(); rst3 <- list()
split_r <- 0.7
n_iter <- 100

for (k in 1:3) {
  dat <- as.matrix(dtlist[[k]][[1]])
  yvar <- as.double(dtlist[[k]][[2]])
  g_idx <- dtlist[[k]][[3]] # group index

  for (h in 1:3) {
    for (j in 1:n_iter) {
      set.seed(h * j * 12345)

      tr_idx <- c(
        sample(c(which(yvar == 0)), table(yvar)[1] * split_r),
        sample(c(which(yvar == 1)), table(yvar)[2] * split_r)
      )
      x <- as.matrix(dat[tr_idx, ])
      xx <- as.matrix(dat[-tr_idx, ])
      y <- as.double(yvar)[tr_idx]
      yy <- as.double(yvar)[-tr_idx]

      # 10-fold CV
      cv_rst <- grpreg::cv.grpreg(X = x,
                                  y = y,
                                  family = "binomial",
                                  group = g_idx,
                                  penalty = pen[h],
                                  alpha = aph[h])
      pred_y <- predict(cv_rst,
                       xx,
                       type = "class",
                       s = cv_rst$lamgda.min) # to obtain predicted values of the response variable
      sltvar <- colnames(dat)[which(coef(cv_rst,
                                         s = cv_rst$lamgda.min) != 0) - 1] # to select variables of nonzero coefficients

      # prediction measures
      perff <- table(yy, pred_y)
      if (dim(perff)[2] == 1) {
        kpp <- NA
        acc <- NA
      } else{
      kpp <- psych::cohen.kappa(perff)$kappa # Kappa
      acc <- (perff[1, 1] + perff[2, 2]) / sum(perff) # accuracy
      }
      auc <- pROC::auc(yy, pred_y, direction="<", levels=c(0,1)) # AUC
      
      auk <- c(acc, auc, kpp)
      rst1[[j]] <- list("prediction"=auk, "sltvar"=sltvar)
    }
   rst2[[h]] <- rst1
  }
  names(rst2) <- c("grLasso", "grEnet", "grMnet")
  rst3[[k]] <- rst2
}
names(rst3) <- mss


########### Random forest ###########

# load data
load("kcyps_rf.RData")

auk <- c()
split_r <- 0.7
n_iter <- 5

for (k in 1:3) {
  dat <- as.matrix(dtlist[[k]][[1]])
  yvar <- as.double(dtlist[[k]][[2]])

  rf_mt <- sqrt(length(dtlist[[k]][[1]])) # the square-root of number of predictors

  for (j in 1:n_iter) {
    set.seed(k * j * 12345)

    tr_idx <- c(sample(c(which(yvar == 0)), table(yvar)[1] * split_r),
                sample(c(which(yvar == 1)), table(yvar)[2] * split_r))
    x <- as.matrix(dat[tr_idx, ])
    xx <- as.matrix(dat[-tr_idx, ])
    y <- as.double(yvar)[tr_idx]
    yy <- as.double(yvar)[-tr_idx]

    r_fit <- randomForest::randomForest(x,
                                        as.factor(y),
                                        importance = T,
                                        mtry = rf_mt)
    pred_y <- predict(r_fit, xx)

    perff <- table(yy, pred_y)
    if (dim(perff)[2] == 1) {
      kpp <- NA
      acc <- NA
    } else{
    kpp <- psych::cohen.kappa(perff)$kappa # kappa
    acc <- (perff[1, 1] + perff[2, 2]) / sum(perff) # accuracy
    }
    auc <- pROC::auc(yy, as.numeric(pred_y), direction="<", levels=c(0,1)) # AUC

    auk_rst <- c(acc, auc, kpp)
    auk <- rbind(auk, auk_rst)
  }
}
