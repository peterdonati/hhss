# Tests for est() function #####################################################

library(hss)

init <- pop(5000, 0.7, success1 = 0.25, success0 = 0.6)

simpleex1 <- simple(init, sample = 0.4, resp = 0.3, bias = 1, times = 10)
simpleex2 <- simple(init,
                    sample = 0.4,
                    resp = c(0.3, 0.6, 0.9),
                    bias = seq(1, 1.2, 0.1),
                    fus = TRUE,
                    fus_scale = 0.5,
                    times = 10)

mandex1 <- mand(init, c(0.3, 0.6, 0.9), seq(1, 1.3, 0.1), times = 4)
mandex2 <- mand(init, c(0.3, 0.6, 0.9), seq(1, 1.3, 0.1),  fus = TRUE,
                fus_sample = 0.5, fus_scale = 0.6, times = 4)

volex1 <- vol(init, c(0.3, 0.6, 1), seq(1, 1.2, 0.1), times = 2)
volex2 <- vol(init, c(0.3, 0.6, 1), seq(1, 1.2, 0.1), fus = TRUE,
              fus_sample = 0.5, fus_scale = 0.7, times = 2)

# Estimating mand() outputs:

test_that(
  est(mandex1)
)
