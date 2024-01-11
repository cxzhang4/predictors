library(checkmate)
library(testthat)

#' Fit an OLS multiple regression model
#'
#' @param data A data frame with only numeric variables and no missing data.
#' @param response_col_name The name of the response variable.
#'
#' @return An object of class "mlm".
#' @export
#'
#' @examples
mlm <- function(data, response_col_name) {
  # type checks
  assert_data_frame(data, types = "numeric")
  assert_string(response_col_name)

  # missingness check
  assert(!anyMissing(data))

  # check that response variable is valid
  all_var_names <- names(data)
  assert_true(response_col_name %in% all_var_names)

  Y <- data[, response_col_name]
  X_no_intercept <- data[, -which(names(data) == response_col_name)]

  # for simplicity, we only allow numeric variables
  # lapply(X_no_intercept, assert_numeric(., any.missing = FALSE))
  # assert_numeric(Y, any.missing = FALSE)

  n <- nrow(X_no_intercept)
  p <- ncol(X_no_intercept)

  # in this implementation, we only have unique coefficient estimates
  # when the number of coefficients is not greater than number of observations
  assert_false((p + 1) > n)

  X <- cbind(intercept = rep(1, times = n), X_no_intercept) |>
    as.matrix()
  Y <- data[, response_col_name]

  # solve the normal equations to compute the OLS estimate of the coefficients
  I_p <- diag(p + 1)
  XT_X_inv <- solve(a = t(X) %*% X, b = I_p)
  Beta_hat <- as.vector(XT_X_inv %*% t(X) %*% Y)

  # add names to the coefficient estimates
  pred_var_names <- names(X_no_intercept)
  coef_names <- c("(Intercept)", pred_var_names)
  names(Beta_hat) <- coef_names

  structure(list(
    predictors = X_no_intercept,
    response = Y,
    response_col_name = response_col_name,
    coefficients = Beta_hat
  ), class = "mlm")
}

#' Print basic information about an mlm object
#'
#' @param mlm_mod An object of class "mlm"
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.mlm <- function(mlm_mod, ...) {
  cat("Multiple Linear Regression Model\n\n")
  print(mlm_mod$coefficients)
  invisible(mlm_mod)
}

#' Generate predictions from an mlm model
#'
#' @param mlm_mod An object of class "mlm"
#' @param newdata
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
predict.mlm <- function(mlm_mod, newdata, ...) {
  # type checks
  assert_equal(ncol(newdata), ncol(mlm_mod$predictors))
  assert_numeric(X_no_intercept, any.missing = FALSE)

  intercept_col_name <- names(mlm_mod$coefficients)[1]
  predictors <- cbind(intercept = rep(1, times = nrow(mlm_mod$predictors)), newdata)
  setnames(predictors, "intercept", intercept_col_name)

  # Y = X * Beta
  predictors %*% coefficientsmlm_mod$coefficients
}
