# Tests for survey functions ###################################################

library(hss)

init <- pop(5000, 0.7, success1 = 0.25, success0 = 0.6)

# simple() tests ===============================================================

simpleex1 <- simple(init, sample = 0.4, resp = 0.3, bias = 1, times = 10)
simpleex2 <- simple(init,
                    sample = 0.4,
                    resp = c(0.3, 0.6, 0.9),
                    bias = seq(1, 1.2, 0.1),
                    fus = TRUE,
                    fus_scale = 0.5,
                    times = 10)

test_that("simple() gives expected outputs",{
  expect_length(simpleex1, 10)
  expect_length(simpleex1[[1]], 10)
  expect_equal(nrow(simpleex1[[1]]), 5000)
  expect_equal(nrow(simpleex2[[1]]), 5000 * 3 * 3)
  expect_length(simpleex2[[1]], 13)
  expect_true("fus_resp" %in% names(simpleex2[[1]]))
  expect_false("fus_resp" %in% names(simpleex1[[1]]))
})

test_that("simple gives conditions", {
  expect_error(
    simple(init,
           sample = 500,
           resp = c(0.3, 0.6, 0.9),
           bias = seq(1, 1.2, 0.1))
    )
  expect_error(
    simple(init,
           sample = 0.5,
           resp = c(1.3, 0.6, 0.9),
           bias = seq(1, 1.2, 0.1))
  )
  expect_error(
    simple(init,
           sample = 0.5,
           resp = c(0.3, 0.6, 0.9),
           bias = seq(1, 1.2, 0.1),
           fus = TRUE)
  )
  expect_error(
    simple(init,
           sample = 0.5,
           resp = c(0.3, 0.6, 0.9),
           bias = seq(1, 1.2, 0.1),
           fus_scale = 0.7)
  )
  expect_message(
    simple(init,
           sample = 0.5,
           resp = c(0.3, 0.6, 0.9),
           bias = seq(0.9, 1.2, 0.1))
  )
  expect_message(
    simple(init,
           sample = 0.5,
           resp = c(0.3, 0.6, 0.9),
           bias = seq(0.9, 1.2, 0.1),
           fus = TRUE,
           fus_scale = 1.4)
  )
})

# mand () tests ================================================================

mandex1 <- mand(init, c(0.3, 0.6, 0.9), seq(1, 1.3, 0.1), times = 4)
mandex2 <- mand(init, c(0.3, 0.6, 0.9), seq(1, 1.3, 0.1),  fus = TRUE,
                fus_sample = 0.5, fus_scale = 0.6, times = 4)

test_that("mand() gives expected outputs", {
  expect_length(mandex1, 4)
  expect_equal(nrow(mandex1[[1]]), 5000 * 3)
  expect_length(mandex1[[1]], 7)
  expect_length(mandex2, 4)
  expect_equal(nrow(mandex2[[1]]), 5000 * 3 * 4)
  expect_length(mandex2[[1]], 12)
  expect_true("fus_resp" %in% names(mandex2[[1]]))
  expect_false("fus_resp" %in% names(mandex1[[1]]))
})

test_that("mand() gives conditions", {
  expect_error(
    mand(init, c(1.3, 0.6, 0.9), seq(1, 1.3, 0.1), times = 4)
    )
  expect_error(
    mand(init, c(0.3, 0.6, 0.9), seq(1, 1.3, 0.1), fus = T,
         fus_sample = 1.2, fus_scale = 0.7, times = 4)
  )
  expect_error(
    mand(init, c(0.3, 0.6, 0.9), seq(1, 1.3, 0.1), fus = T,
         fus_sample = 0.5, fus_scale = NULL, times = 4)
  )
  expect_error(
    mand(init, c(0.3, 0.6, 0.9), seq(1, 1.3, 0.1), fus = T,
         fus_sample = NULL, fus_scale = 0.6, times = 4)
  )
  expect_error(
    mand(init, c(0.3, 0.6, 0.9), seq(1, 1.3, 0.1), fus = FALSE,
         fus_sample = NULL, fus_scale = 0.6, times = 4)
  )
  expect_error(
    mand(init, c(0.3, 0.6, 0.9), seq(1, 1.3, 0.1), fus = FALSE,
         fus_sample = 0.5, fus_scale = NULL, times = 4)
  )
  expect_message(
    mand(init, c(0.3, 0.6, 0.9), seq(1, 1.3, 0.1), fus = TRUE,
         fus_sample = 0.5, fus_scale = 1.4, times = 4)
  )
})

# vol() tests ==================================================================

volex1 <- vol(init, c(0.3, 0.6, 1), seq(1, 1.2, 0.1), times = 2)
volex2 <- vol(init, c(0.3, 0.6, 1), seq(1, 1.2, 0.1), fus = TRUE,
              fus_sample = 0.5, fus_scale = 0.7, times = 2)

test_that("vol() gives expected outputs", {
  expect_length(volex1, 2)
  expect_length(volex1[[1]], 9)
  expect_equal(nrow(volex1[[1]]), 5000 * 3 * 3)
  expect_length(volex2[[1]], 13)
  expect_equal(nrow(volex2[[1]]), 5000 * 3 * 3)
  expect_true("fus_resp" %in% names(volex2[[1]]))
  expect_false("fus_resp" %in% names(volex1[[1]]))
})

test_that("vol() gives conditions", {
  expect_error(vol(init, c(0.3, 1.6, 1), seq(1, 1.2, 0.1), times = 2))
  expect_error(
    vol(init, c(0.3, 0.6, 1), seq(1, 1.2, 0.1), fus = TRUE,
        fus_sample = 1.5, fus_scale = 0.7, times = 2)
  )
  expect_error(
    vol(init, c(0.3, 0.6, 1), seq(1, 1.2, 0.1), fus = TRUE,
        fus_sample = 0.5, fus_scale = NULL, times = 2)
  )
  expect_error(
    vol(init, c(0.3, 0.6, 1), seq(1, 1.2, 0.1), fus = TRUE,
        fus_sample = NULL, fus_scale = 0.7, times = 2)
  )
  expect_error(
    vol(init, c(0.3, 0.6, 1), seq(1, 1.2, 0.1), fus = FALSE,
        fus_sample = 0.5, fus_scale = NULL, times = 2)
  )
  expect_error(
    vol(init, c(0.3, 0.6, 1), seq(1, 1.2, 0.1), fus = FALSE,
        fus_sample = NULL, fus_scale = 0.7, times = 2)
  )
  expect_message
  (vol(init, c(0.3, 0.6, 1), seq(1, 1.2, 0.1), fus = TRUE,
       fus_sample = 0.5, fus_scale = 1.7, times = 2)
  )
  expect_message(
    vol(init, c(0.3, 0.6, 1), seq(0.9, 1.2, 0.1), fus = TRUE,
        fus_sample = 0.5, fus_scale = 0.7, times = 2)
  )
})







