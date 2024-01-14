# instantiate models for the following cases
# TODO: consider testing combinations of these cases

test_that("mlm works on a small dataset", {
  df_3 <- data.frame(x = c(-2, 0, 4),
                     y = c(-2, 3, 2))

  mlm_mod_df_3 <- mlm(data = df_3, response_col_name = "y")
  lm_mod_df_3 <- lm(y ~ ., data = df_3)

  testthat::expect_equal(mlm_mod_df_3$coefficients, lm_mod_df_3$coefficients,
                         tolerance = coef_tolerance)

  checkmate::assert(ncol(df_3) - 1 == 1)
  testthat::expect_output(print(mlm_mod_df_3),
                          "^Multiple Linear Regression Model\n\n \\(Intercept\\) +cyl +wt *\n *[-0-9.eE]+ +[-0-9.eE]+ *$")


})


# working models: p < n
# mtcars
?checkmate::assert(ncol(mtcars) < nrow(mtcars))
mlm_mod_mtcars <- mlm(data = mtcars, response_col_name = "mpg")
lm_mod_mtcars <- lm(mpg ~ ., data = mtcars)
ncol(mtcars) - 1
mtcars_small <- mtcars[, c("mpg", "cyl", "wt")]
mlm_mod_mtcars_small <- mlm(data = mtcars_small, response_col_name = "mpg")
lm_mod_mtcars_small <- lm(mpg ~ ., data = mtcars_small)

# toy data: 3 observations
df_3 <- data.frame(x = c(-2, 0, 4),
                   y = c(-2, 3, 2))

mlm_mod_df_3 <- mlm(data = df_3, response_col_name = "y")
lm_mod_df_3 <- lm(y ~ ., data = df_3)

# tests for coefficients
coef_tolerance <- 1e-5

testthat::expect_equal(mlm_mod_mtcars$coefficients, lm_mod_mtcars$coefficients,
                       tolerance = coef_tolerance)
testthat::expect_equal(mlm_mod_df_3$coefficients, lm_mod_df_3$coefficients,
                       tolerance = coef_tolerance)

# tests for printing
checkmate::assert(ncol(mtcars) - 1 == 10)
testthat::expect_output(print(mlm_mod_mtcars),
                        "^Multiple Linear Regression Model\n\n \\(Intercept\\) +cyl +wt *\n *[-0-9.eE]+ +[-0-9.eE]+ +[-0-9.eE]+ [-0-9.eE]+ [-0-9.eE]+ [-0-9.eE]+ [-0-9.eE]+ [-0-9.eE]+ [-0-9.eE]+ [-0-9.eE]+ [-0-9.eE]+ *$")

checkmate::assert(ncol(mtcars_small) - 1 == 2)
testthat::expect_output(print(mlm_mod_mtcars_small),
                        "^Multiple Linear Regression Model\n\n \\(Intercept\\) +cyl +wt *\n *[-0-9.eE]+ +[-0-9.eE]+ +[-0-9.eE]+ *$")

checkmate::assert(ncol(df_3) - 1 == 1)
testthat::expect_output(print(mlm_mod_df_3),
                        "^Multiple Linear Regression Model\n\n \\(Intercept\\) +cyl +wt *\n *[-0-9.eE]+ +[-0-9.eE]+ *$")

# do not override the base print function
testthat::expect_equal(print, base::print)

# TODO: tests for predictions
expect_error(predict(mlm_mod_df_3, newdata = df_3))
df_3_noresponse <- as.data.frame(df_3[, "x"], col.names = c("x"))
names(df_3_noresponse) = "x"
mlm_preds_df_3 <- predict(mlm_mod_df_3, newdata = df_3_noresponse)
lm_preds_df_3 <- predict(lm_mod_df_3, newdata = df_3_noresponse)
expect_equal(mlm_preds_df_3, lm_preds_df_3)

expect_error(predict(mlm_mod_mtcars_small, newdata = mtcars_small))
mtcars_small_noresponse <- as.data.frame(x = mtcars_small[, -which(names(mtcars_small) == "mpg")])
mlm_preds_mtcars_small <- predict(mlm_mod_mtcars_small, newdata = mtcars_small_noresponse)
lm_preds_mtcars_small <- predict(lm_mod_mtcars_small, newdata = mtcars_small_noresponse)
expect_equal(mlm_preds_mtcars_small, lm_preds_mtcars_small)

expect_error(predict(mlm_mod_mtcars, newdata = mtcars))
mtcars_noresponse <- as.data.frame(x = mtcars[, -which(names(mtcars) == "mpg")])
mlm_preds_mtcars <- predict(mlm_mod_mtcars, newdata = mtcars_noresponse)
lm_preds_mtcars <- predict(lm_mod_mtcars, newdata = mtcars_noresponse)
expect_equal(mlm_preds_mtcars, lm_preds_mtcars)

# p == n
mtcars_square_idx <- sample(1:nrow(mtcars), ncol(mtcars) - 1, replace = FALSE)
mtcars_square <- mtcars[mtcars_square_idx, ]
checkmate::assert(ncol(mtcars_square) - 1 == nrow(mtcars_square))

expect_error(mlm(data = mtcars_square, response_col_name = "mpg"))

# p > n
mtcars_highdim_idx <- sample(1:nrow(mtcars), ncol(mtcars) - 2, replace = FALSE)
mtcars_highdim <- mtcars[mtcars_highdim_idx, ]
checkmate::assert(ncol(mtcars_highdim) - 1 > nrow(mtcars_highdim))

expect_error(mlm(data = mtcars_highdim, response_col_name = "mpg"))

# missing data in a predictor
mtcars_missing_predictor <- mtcars
mtcars_missing_predictor[1, "cyl"] = NA
assert_false(all(sapply(mtcars_missing_predictor, anyMissing)))

expect_error(mlm(data = mtcars_missing_predictor, response_col_name = "mpg"))

# missing data in the response
mtcars_missing_response <- mtcars
mtcars_missing_response[1, "mpg"] = NA
assert_false(all(sapply(mtcars_missing_response, anyMissing)))

expect_error(mlm(data = mtcars_missing_response, response_col_name = "mpg"))

# non-numeric predictor
mtcars_nonnum_predictor <- mtcars
mtcars_nonnum_predictor[, "vs"] <- as.character(mtcars_nonnum_predictor[, "vs"])
checkmate::assert(typeof(mtcars_nonnum_predictor$vs) == "character")

expect_error(mlm(data = mtcars_nonnum_predictor, response_col_name = "mpg"))

# non-numeric response
mtcars_nonnum_response <- mtcars
mtcars_nonnum_predictor[, "mpg"] <- as.character(mtcars_nonnum_predictor[, "mpg"])
checkmate::assert(typeof(mtcars_nonnum_predictor$mpg) == "character")

expect_error(mlm(data = mtcars_nonnum_predictor, response_col_name = "mpg"))
