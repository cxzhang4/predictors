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
  checkmate::assert_data_frame(data, types = "numeric")
  checkmate::assert_string(response_col_name)

  # missingness check
  checkmate::assert(all(!sapply(data, checkmate::anyMissing)))

  checkmate::assert(ncol(data) >= 2)
  checkmate::assert(ncol(data) <= nrow(data))

  # check that response variable is valid
  all_var_names <- names(data)
  checkmate::assert_true(response_col_name %in% all_var_names)

  predictor_var_names <- -which(names(data) == response_col_name)

  Y <- data[, response_col_name]
  X_no_intercept <- data[, predictor_var_names]

  # accessing a single column returns a vector, but we want a dataframe
  if (is.vector(X_no_intercept)) {
    X_no_intercept <- data.frame(x = X_no_intercept)
    names(X_no_intercept) <- names(data)[predictor_var_names]
  }

  n <- nrow(X_no_intercept)
  p <- ncol(X_no_intercept)

  # in this implementation, we only have unique coefficient estimates
  # when the number of coefficients is not greater than number of observations
  checkmate::assert_false(p > n)

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

#' Generate predictions from an mlm model
#'
#' @param object An object of class "mlm"
#' @param newdata
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
predict.mlm <- function(object, newdata, ...) {
  # type checks
  checkmate::assert_data_frame(newdata, types = "numeric")
  checkmate::assert(ncol(newdata) == ncol(object$predictors))

  # missingness check
  checkmate::assert_false(all(sapply(newdata, checkmate::anyMissing)))

  # dimensionality check
  checkmate::assert(ncol(newdata) == ncol(object$predictors))

  intercept_col_name <- names(object$coefficients)[1]
  predictors <- cbind(intercept = rep(1, times = nrow(object$predictors)), newdata)
  # setNames(predictors, "intercept", intercept_col_name)

  # column ordering check
  testthat::expect_equal(names(newdata), names(object$predictors))

  # Y = X * Beta
  predictors_norownames <- predictors
  predictors_matrix <- as.matrix(predictors_norownames)
  ret_vector <- as.vector(predictors_matrix %*% object$coefficients)
  # names(ret_vector) = ifelse(is.null(rowNames(newdata)),
  #                            as.character(1:length(ret_vector)),
  #                            rowNames(newdata))
  names(ret_vector) <- rownames(newdata)
  ret_vector
}
