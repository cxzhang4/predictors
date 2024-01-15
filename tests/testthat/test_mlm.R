# instantiate models for the following cases
# TODO: consider testing combinations of these cases

test_that("mlm computations works on a small dataset", {
  df_3 <- data.frame(x = c(-2, 0, 4),
                     y = c(-2, 3, 2))

  mlm_mod_df_3 <- mlm(data = df_3, response_col_name = "y")
  lm_mod_df_3 <- lm(y ~ ., data = df_3)

  coef_tolerance <- 1e-5
  testthat::expect_equal(mlm_mod_df_3$coefficients, lm_mod_df_3$coefficients,
                         tolerance = coef_tolerance)

  checkmate::assert(ncol(df_3) - 1 == 1)
  # testthat::expect_output(print(mlm_mod_df_3),
  #                         "^Multiple Linear Regression Model\n\n \\(Intercept\\) +cyl +wt *\n *[-0-9.eE]+ +[-0-9.eE]+ *$")

  testthat::expect_equal(mlm_mod_df_3$coefficients, lm_mod_df_3$coefficients,
                         tolerance = coef_tolerance)

  testthat::expect_error(predict(mlm_mod_df_3, newdata = df_3))
  df_3_noresponse <- as.data.frame(df_3[, "x"], col.names = c("x"))
  names(df_3_noresponse) = "x"

  mlm_preds_df_3 <- predict(mlm_mod_df_3, newdata = df_3_noresponse)
  lm_preds_df_3 <- predict(lm_mod_df_3, newdata = df_3_noresponse)

  testthat::expect_equal(mlm_preds_df_3, lm_preds_df_3)
})

test_that("mlm computations work on mtcars with 2 predictors", {
  mtcars_small <- mtcars[, c("mpg", "cyl", "wt")]

  mlm_mod_mtcars_small <- mlm(data = mtcars_small, response_col_name = "mpg")
  lm_mod_mtcars_small <- lm(mpg ~ ., data = mtcars_small)

  coef_tolerance <- 1e-5
  testthat::expect_equal(mlm_mod_mtcars_small$coefficients, lm_mod_mtcars_small$coefficients,
                         tolerance = coef_tolerance)

  testthat::expect_equal(mlm_mod_mtcars_small$coefficients, lm_mod_mtcars_small$coefficients,
                         tolerance = coef_tolerance)

  expect_error(predict(mlm_mod_mtcars_small, newdata = mtcars_small))
  mtcars_small_noresponse <- as.data.frame(x = mtcars_small[, -which(names(mtcars_small) == "mpg")])

  mlm_preds_mtcars_small <- predict(mlm_mod_mtcars_small, newdata = mtcars_small_noresponse)
  lm_preds_mtcars_small <- predict(lm_mod_mtcars_small, newdata = mtcars_small_noresponse)
  expect_equal(mlm_preds_mtcars_small, lm_preds_mtcars_small)
})

test_that("mlm computations work on mtcars", {
  mlm_mod_mtcars <- mlm(data = mtcars, response_col_name = "mpg")
  lm_mod_mtcars <- lm(mpg ~ ., data = mtcars)

  coef_tolerance <- 1e-5
  testthat::expect_equal(mlm_mod_mtcars$coefficients, lm_mod_mtcars$coefficients,
                         tolerance = coef_tolerance)

  testthat::expect_equal(mlm_mod_mtcars$coefficients, lm_mod_mtcars$coefficients,
                         tolerance = coef_tolerance)

  expect_error(predict(mlm_mod_mtcars, newdata = mtcars))
  mtcars_noresponse <- as.data.frame(x = mtcars[, -which(names(mtcars) == "mpg")])

  mlm_preds_mtcars <- predict(mlm_mod_mtcars, newdata = mtcars_noresponse)
  lm_preds_mtcars <- predict(lm_mod_mtcars, newdata = mtcars_noresponse)

  testthat::expect_equal(mlm_preds_mtcars, lm_preds_mtcars)
})

test_that("mlm throws an error for p == n predictors", {
  mtcars_square_idx <- sample(1:nrow(mtcars), ncol(mtcars) - 1, replace = FALSE)
  mtcars_square <- mtcars[mtcars_square_idx, ]
  checkmate::assert(ncol(mtcars_square) - 1 == nrow(mtcars_square))

  testthat::expect_error(mlm(data = mtcars_square, response_col_name = "mpg"))
})

test_that("mlm throws an error for p > n predictors", {
  mtcars_highdim_idx <- sample(1:nrow(mtcars), ncol(mtcars) - 2, replace = FALSE)
  mtcars_highdim <- mtcars[mtcars_highdim_idx, ]
  checkmate::assert(ncol(mtcars_highdim) - 1 > nrow(mtcars_highdim))

  testthat::expect_error(mlm(data = mtcars_highdim, response_col_name = "mpg"))
})

test_that("mlm throws an error when a predictor has missing data", {
  mtcars_missing_predictor <- mtcars
  mtcars_missing_predictor[1, "cyl"] = NA
  testthat::expect_false(all(sapply(mtcars_missing_predictor, anyMissing)))

  testthat::expect_error(mlm(data = mtcars_missing_predictor, response_col_name = "mpg"))
})

test_that("mlm throws an error when the response has missing data", {
  mtcars_missing_response <- mtcars
  mtcars_missing_response[1, "mpg"] = NA
  testthat::expect_false(all(sapply(mtcars_missing_response, anyMissing)))

  testthat::expect_error(mlm(data = mtcars_missing_response, response_col_name = "mpg"))
})

test_that("mlm throws an error when a predictor is non-numeric", {
  mtcars_nonnum_predictor <- mtcars
  mtcars_nonnum_predictor[, "vs"] <- as.character(mtcars_nonnum_predictor[, "vs"])
  checkmate::assert(typeof(mtcars_nonnum_predictor$vs) == "character")

  testthat::expect_error(mlm(data = mtcars_nonnum_predictor, response_col_name = "mpg"))
})

test_that("mlm throws an error when the response is non-numeric", {
  mtcars_nonnum_response <- mtcars
  mtcars_nonnum_response[, "mpg"] <- as.character(mtcars_nonnum_response[, "mpg"])
  checkmate::assert(typeof(mtcars_nonnum_response$mpg) == "character")

  testthat::expect_error(mlm(data = mtcars_nonnum_response, response_col_name = "mpg"))
})
