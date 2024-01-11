# instantiate models for the following cases
# TODO: combinations of these cases

# p < n
mlm_mod <- mlm(data = mtcars, response_col_name = "mpg")
lm_mod <- lm(mpg ~ ., data = mtcars)

# p == n
mtcars_square_idx <- sample(1:nrow(mtcars), ncol(mtcars), replace = FALSE)
mtcars_square <- mtcars[mtcars_square_idx, ]

# p > n
mtcars_highdim_idx <- sample(1:nrow(mtcars), ncol(mtcars) - 1, replace = FALSE)
mtcars_highdim <- mtcars[mtcars_himdim_idx, ]

# non-numeric predictor
mtcars_non_num <- mtcars

# non-numeric response

# tests for printing
testthat::expect_output(print(mlm(data = mtcars, response = "")))

testthat::expect_equal(print, base::print)

# tests for coefficients





# tests for predictions




