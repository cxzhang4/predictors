library(checkmate)
library(testthat)
library(data.table)

mlm <- function(data, response_col_name) {
  # type checks
  assert_data_frame(data)
  assert_string(response_col_name)

  # check that response variable is valid
  all_var_names <- names(data)
  assert_true(response_col_name %in% all_var_names)

  Y <- data[, response_col_name]
  X_no_intercept <- data[, -response_col_name]

  # for simplicity, we only allow numeric variables
  assert_numeric(X_no_intercept, any.missing = FALSE)
  assert_numeric(Y, any.missing = FALSE)

  n <- nrow(X_no_intercept)
  p <- ncol(X_no_intercept)

  # in this implementation, we only have unique coefficient estimates
  # when the number of coefficients is not greater than number of observations
  assert_false((p + 1) > n)

  X <- cbind(intercept = rep(1, times = n), X_no_intercept)
  Y <- data[, response_col_name]

  # solve the normal equations to compute the OLS estimate of the coefficients
  I_p <- diag(p)
  XT_X_inv <- solve(a = t(X) %*% X, b = I_p)
  Beta_hat <- XT_X_inv %*% t(X) %*% Y

  # add names to the coefficient estimates
  pred_var_names <- names(X_no_intercept)
  coef_names <- c("(Intercept)", pred_var_names)
  names(Beta_hat) <- coef_names

  structure(list(
    predictors = X_no_intercept,
    response = Y,
    coefficients = Beta_hat,
  ), class = "mlm")
}

print.mlm <- function(mlm_mod, ...) {
  cat("Multiple Linear Regression Model")
  print(mlm_mod$coefficients)
  invisible(mlm_mod)
}

predict.mlm(mlm_mod, newdata, ...) {
  # type checks
  assert_equal(ncol(newdata), ncol(mlm_mod$predictors))
  assert_numeric(X_no_intercept, any.missing = FALSE)

  intercept_col_name <- names(mlm_mod$coefficients)[1]
  predictors <- cbind(intercept = rep(1, times = nrow(mlm_mod$predictors)), newdata)
  setnames(predictors, "intercept", intercept_col_name)

  # Y = X * Beta
  predictors %*% coefficientsmlm_mod$coefficients
}
