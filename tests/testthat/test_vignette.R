context("Vignette exists")
library(platowork)

test_that("vignette, testing-platoworks, exists", {
  expect_equal(class(utils::vignette("testing-platowork")), "vignette")
})
#> Test passed
