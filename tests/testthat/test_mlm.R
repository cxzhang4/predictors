# instantiate models for the following cases
# TODO: consider testing combinations of these cases

# working models: p < n
# mtcars
assert(ncol(mtcars) < nrow(mtcars))
mlm_mod_mtcars <- mlm(data = mtcars, response_col_name = "mpg")
lm_mod_mtcars <- lm(mpg ~ ., data = mtcars)

# toy data:
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
testthat::expect_output(print(mlm(data = mtcars, response_col_name = mpg)),
                        "^Multiple Linear Regression Model\n\n *(Intercept) ")

testthat::expect_equal(print, base::print)

# tests for predictions

# p == n
mtcars_square_idx <- sample(1:nrow(mtcars), ncol(mtcars) - 1, replace = FALSE)
mtcars_square <- mtcars[mtcars_square_idx, ]
assert(ncol(mtcars_square) - 1 == nrow(mtcars_square))

expect_error(mlm(data = mtcars_square, response_col_name = "mpg"))

# p > n
mtcars_highdim_idx <- sample(1:nrow(mtcars), ncol(mtcars) - 2, replace = FALSE)
mtcars_highdim <- mtcars[mtcars_highdim_idx, ]
assert(ncol(mtcars_highdim) - 1 > nrow(mtcars_highdim))

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
assert(typeof(mtcars_nonnum_predictor$vs) == "character")

expect_error(mlm(data = mtcars_nonnum_predictor, response_col_name = "mpg"))

# non-numeric response
mtcars_nonnum_response <- mtcars
mtcars_nonnum_predictor[, "mpg"] <- as.character(mtcars_nonnum_predictor[, "mpg"])
assert(typeof(mtcars_nonnum_predictor$mpg) == "character")

expect_error(mlm(data = mtcars_nonnum_predictor, response_col_name = "mpg"))
