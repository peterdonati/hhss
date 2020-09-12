# Tests for pop() function #####################################################

library(hss)

test_that("length of outputs is correct", {
  expect_length(pop(n = 1000, success1 = 0.3), 4)
  expect_length(pop(n = 1000, split = 0.5, success1 = 0.3, success0 = 0.6), 4)
  expect_equal(nrow(pop(n = 15000, success1 = 0.3)), 15000)
})

test_that("conditions working", {
  expect_error(pop(n = 1000, split = 1.5, success1 = 0.3, success0 = 0.6))
  expect_error(pop(n = 1000, split = 0.5, success1 = 8.3, success0 = 0.6))
  expect_error(pop(n = 1000, split = 0.5, success1 = 0.3, success0 = 4.6))
  expect_warning(pop(n = 1000, split = 0.5, success1 = 0.3))
  expect_warning(pop(n = 1000, split = 1, success1 = 0.3, success0 = 0.6))
})
