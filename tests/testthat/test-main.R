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




