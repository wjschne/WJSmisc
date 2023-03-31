library(testthat)
# rbeta_ms
test_that("mu between 0 and 1", {
  expect_error(rbeta_ms(mu = 2))
  expect_error(rbeta_ms(mu = 0))
})

test_that("sigma less than mu * (1 - mu)", {
  expect_error(rbeta_ms(sigma = 2))
})

test_that("equal mu if sigma = 0", {
  expect_equal(rbeta_ms(mu = 0.6, sigma = 0), 0.6)
})

test_that("between 0 and 1", {
  expect_true(all(dplyr::between(rbeta_ms(100, mu = 0.6, sigma = 0.2),0,1)))
})


test_that("remove leading zero", {
  expect_equal(remove_leading_zero(0.5, digits = 2), ".50")
  expect_equal(remove_leading_zero(-0.5, digits = 2), "-.50")
  expect_equal(remove_leading_zero(1.5, digits = 2), "1.50")
  expect_equal(remove_leading_zero(0, digits = 2), ".00")
})


test_that("prob_label", {
  expect_equal(prob_label(c(.0010, .00123, .010, .09, .091, .4, .6, 0.988, .99, .998, .999, .99986), digits = 2),
               c(".001", ".0012", ".01", ".090", ".091", ".40", ".60", ".99", ".99", ".998", ".999", ".9999"))
})


test_that("rotate2dmatrix",{
  # x and origin must be numeric
  expect_error(rotate2dmatrix(c("x", "y"), theta = pi),
               regexp = "x must be numeric")
  expect_error(rotate2dmatrix(c(0,1), theta = pi, origin = c("x", "y")),
               regexp = "origin must be numeric")

  # x is vector but not of length 2
  expect_error(rotate2dmatrix(c(0,1,2), theta = pi),
               regexp = "x must be a 2-column matrix or a length-2 vector")
  # x is a matrix but not with 2 columns
  expect_error(rotate2dmatrix(matrix(c(0,1)), theta = pi),
               regexp = "x must be a 2-column matrix or a length-2 vector")


  # origin is a vector but not of length 2
  expect_error(rotate2dmatrix(c(0,1),
                              theta = pi,
                              origin = c(0,1,2)),
               regexp = "origin must be a 2-column matrix or a length-2 vector")
  # origin is a matrix but not with 2 columns
  expect_error(
    rotate2dmatrix(matrix(c(0,1), ncol = 2),
                   theta = pi,
                   origin = matrix(2, nrow = 2, ncol = 1)),
    regexp = "origin must be a 2-column matrix or a length-2 vector")
})



