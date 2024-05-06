library(caret)
library(glmnet)
source("~/Library/CloudStorage/OneDrive-Personal/financial_data_mining/final_project/cooperative-learning/cooperative_learning/cooperative_regression_function.R")

simulate_data <- function(n=5000, px=500, pz=500, p_imp=30, sigma=39, 
                          sy=1, sx=3, sz=3, u_std=1, factor_strength=7,
                          train_frac=0.1, nfolds = 20){
  
  # Simulate data based on the factor model
  x = matrix(rnorm(n*px), n, px)
  z = matrix(rnorm(n*pz), n, pz)
  U = matrix(rep(0, n*p_imp), n, p_imp)
  
  for (m in seq(p_imp)){
    u = rnorm(n, sd = u_std)
    x[, m] = x[, m] + sx*u
    z[, m] = z[, m] + sz*u
    U[, m] = U[, m] + sy*u
  }
  x = scale(x, T, F)
  z = scale(z, T, F)
  
  beta_U = c(rep(factor_strength, p_imp))
  mu_all = U %*% beta_U
  y = mu_all + sigma * rnorm(n) 
  
  snr = var(mu_all) / var(y-mu_all)
  cat("", fill=T)
  cat(c("snr =",snr),fill=T)
  cat("",fill=T)
  
  # Split training and test sets
  smp_size_train = floor(train_frac * nrow(x)) 
  train_ind = sort(sample(seq_len(nrow(x)), size = smp_size_train))
  test_ind = setdiff(seq_len(nrow(x)), train_ind)
  
  colnames(x) = seq(ncol(x))
  colnames(z) = seq(ncol(z))
  
  train_X_raw <- x[train_ind, ]
  test_X_raw <- x[test_ind, ]
  train_Z_raw <- z[train_ind, ]
  test_Z_raw <- z[test_ind, ]
  train_y <- y[train_ind, ]
  test_y <- y[test_ind, ]
  
  preprocess_values_train = preProcess(train_X_raw, method = c("center", "scale"))
  train_X = predict(preprocess_values_train, train_X_raw)
  test_X = predict(preprocess_values_train, test_X_raw)
  
  preprocess_values_train_Z = preProcess(train_Z_raw, method = c("center", "scale"))
  train_Z = predict(preprocess_values_train_Z, train_Z_raw)
  test_Z = predict(preprocess_values_train_Z, test_Z_raw)
  
  foldid = sample(rep_len(1:nfolds, dim(train_X)[1]))
  
  return(data=list(x=x, z=z, y=y, train_X=train_X, train_Z=train_Z, train_y=train_y,
                   test_X=test_X, test_Z=test_Z, test_y=test_y, foldid=foldid))
}



calc_mse <- function(actual, predicted) {
  return(mean((actual - predicted)^2))
}

data = simulate_data()

alpha = 0.5
coop_fit = coop_cv_new(data$train_X,data$train_Z,data$train_y,alpha=alpha,
                       foldid=data$foldid,nfolds=max(data$foldid),
                       pf_values=rep(1,ncol(data$train_X)+ncol(data$train_Z)))
coop_fit

coop_pred_test = cbind(data$test_X, data$test_Z) %*% coop_fit$best_fit$beta + (coop_fit$best_fit$a0*2)
coop_pred_test
calc_mse(coop_pred_test, data$test_y)

early_fusion = cv.glmnet(cbind(data$train_X,data$train_Z), data$train_y, 
                         standardize = F, foldid = data$foldid)
early_pred_test = predict(early_fusion, cbind(data$test_X, data$test_Z), s="lambda.min")
calc_mse(early_pred_test, data$test_y)
par(mfrow=c(1,1))

ccv_1se <- coop_cv_new_1se(x=data$x,z=data$z,y=data$y,alpha=alpha,foldid=data$foldid,nfolds=10,pf_values=coop_fit$pf_values,n_iter=3,fit_mode="1se")
ccv_1se

plot_cv_coop(ccv_1se, title="Cooperative Regression CV Curve", lamx=0.1, lamz=0.2)

# Separate X
separate_X_fit <- cv.glmnet(data$train_X, data$train_y, standardize = FALSE, foldid = data$foldid)
separate_X_pred_test <- predict(separate_X_fit, data$test_X, s="lambda.min")
separate_X_mse <- calc_mse(separate_X_pred_test, data$test_y)
separate_X_mse
# Separate Z
separate_Z_fit <- cv.glmnet(data$train_Z, data$train_y, standardize = FALSE, foldid = data$foldid)
separate_Z_pred_test <- predict(separate_Z_fit, data$test_Z, s="lambda.min")
separate_Z_mse <- calc_mse(separate_Z_pred_test, data$test_y)
separate_Z_mse
# Late fusion
late_X_fit <- cv.glmnet(data$train_X, data$train_y, standardize = FALSE, foldid = data$foldid)
late_X_pred <- predict(late_X_fit, data$test_X, s="lambda.min")
late_Z_fit <- cv.glmnet(data$train_Z, data$train_y, standardize = FALSE, foldid = data$foldid)
late_Z_pred <- predict(late_Z_fit, data$test_Z, s="lambda.min")
late_fusion_pred <- (late_X_pred + late_Z_pred) / 2
late_fusion_mse <- calc_mse(late_fusion_pred, data$test_y)
late_fusion_mse
# Adap Coop
coop_adap_fit <- coop_regression_iter_order(data$x, data$z, data$y, alpha)
coop_adap_fit$fit
# Calculate predictions
# Convert data$test_X and data$test_Z to matrices
test_X_matrix <- as.matrix(data$test_X)
test_Z_matrix <- as.matrix(data$test_Z)

class(data$test_X)
class(coop_adap_fit$thetax)

coop_adap_pred_test <- cbind(data$test_X %*% coop_adap_fit$fit$thetax + coop_adap_fit$fit$intercept_x * 2,
                             data$test_Z %*% coop_adap_fit$fit$thetaz + coop_adap_fit$fit$intercept_z * 2)

coop_adap_mse <- calc_mse(coop_adap_pred_test, data$test_y)
coop_adap_mse
# Create a dataframe for the boxplot
df <- data.frame(
  Method = c("Separate X", "Separate Z", "Early Fusion", "Late Fusion", "Coop", "Adap Coop"),
  MSE = c(separate_X_mse, separate_Z_mse, early_mse, late_fusion_mse, coop_mse, coop_adap_mse)
)

# Create the boxplot
library(ggplot2)
ggplot(df, aes(x = Method, y = MSE, fill = Method)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Comparison of Test MSE among Different Methods",
    x = "Method",
    y = "Test Mean Squared Error (MSE)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Separate X" = "#F8766D", "Separate Z" = "#F8766D",
                               "Early Fusion" = "#00BFC4", "Late Fusion" = "#00BFC4",
                               "Coop" = "#00BA38", "Adap Coop" = "#619CFF"))

