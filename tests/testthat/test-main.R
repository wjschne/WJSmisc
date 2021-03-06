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
