library(checkmate)
library(testthat)

library(data.table)

# error <- function(x, y, beta) {
#   y - (x %*% beta)
# }
#
# squared_error <- function(x, y, beta) {
#   error(x, y, beta) ^ 2
# }

# are_formula_vars_in_data <- function(formula, data) {
#   check_formula(formula)
#   check_data_table(data)
#
#   formula_vars <- all.vars(formula)
#
#   expect_true(all(formula_vars %in% names(data)))
# }

#
mlm <- function(data, target_col_name) {
  # type checks
  check_data_table(data)
  check_string(target_col_name)

  # check that response variable is valid
  all_var_names <- names(data)
  expect_true(target_col_name %in% all_var_names)

  Y <- data[, target_col_name]
  X_no_intercept <- data[, -target_col_name]

  # for simplicity, we only allow numeric variables
  assert_numeric(X_no_intercept, any.missing = FALSE)
  assert_numeric(Y, any.missing = FALSE)

  n <- nrow(X_no_intercept)
  p <- ncol(X_no_intercept)

  # unique coefficient estimates only when number of coefficients is not greater than number of observations
  expect_false((p + 1) > n)

  X <- cbind(intercept = rep(1, times = n), X_no_intercept)
  Y <- data[, target_col_name]

  # solve the normal equations to compute the OLS estimate of the coefficients
  expect_false((p + 1) > n)

  I_p <- diag(p)
  XT_X_inv <- solve(a = t(X) %*% X, b = I_p)

  Beta_hat <- XT_X_inv %*% t(X) %*% Y

  pred_var_names <- names(X)
  coef_names <- c("(Intercept)", pred_var_names)

  structure(list(
    design_mat = X,
    response = Y,
    coefficients = Beta_hat,
  ), class = "mlm")
}
