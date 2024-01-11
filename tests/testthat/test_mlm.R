library(palmerpenguins)

# instantiate models for the following cases
# TODO: consider testing combinations of these cases

# working models: p > n
assert(ncol(mtcars) < nrow(mtcars))
mlm_mod_mtcars <- mlm(data = mtcars, response_col_name = "mpg")
lm_mod_mtcars <- lm(mpg ~ ., data = mtcars)

# non-numeric predictor
assert(ncol(penguins) < nrow(penguins))
mlm_mod_penguins <- mlm(data = penguins, response_col_name = "species")

# non-numeric response
assert(ncol(penguins) < nrow(penguins))
mlm_mod_penguins <- mlm(data = penguins, response_col_name = "species")

# tests for coefficients
testthat::expect_equal(mlm_mod_mtcars$coefficients, lm_mod_mtcars$coefficients)

# tests for printing
testthat::expect_output(print(mlm(data = mtcars, response = "")))

testthat::expect_equal(print, base::print)

# tests for predictions

# p == n
mtcars_square_idx <- sample(1:nrow(mtcars), ncol(mtcars), replace = FALSE)
mtcars_square <- mtcars[mtcars_square_idx, ]

# p > n
mtcars_highdim_idx <- sample(1:nrow(mtcars), ncol(mtcars) - 1, replace = FALSE)
mtcars_highdim <- mtcars[mtcars_highdim_idx, ]
