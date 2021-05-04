context("Data exists")
library(platowork)

test_that("plato has the right dimensions", {
  expect_equal(dim(plato), c(225, 6))
})
#> Test passed
