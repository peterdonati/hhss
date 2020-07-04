# pae functions documentation ================================================
#'
#' Simulate populations and calculate estimates.
#'
#' @name pae
#'
#' @aliases pae_simple pae_mand pae_vol
#'
#' @description
#' The \code{pae} functions simulate \strong{p}opulations \strong{a}nd
#' \strong{e}stimate total harvest all in one step. They are
#' wrappers around their respective \code{\link{pop}} functions and the
#' \code{\link{est}} function. These functions are useful when you do not need
#' the actual population data and would like to skip that intermediate step.
#' \itemize{
#' \item \code{pae_mand()} simulates populations, mandates successful hunters
#' to report their harvest, and creates harvest estimates from responses.
#' \item \code{pae_simple()} simulates populations, surveys them using
#' simple random sampling, and creates harvest estimates from responses.
#' \item \code{pae_vol()} simulates populations, allows both successful and
#' unsuccessful hunters to voluntarily report, and creates harvest estimates
#' from responses.
#' }
#'
#' @param resp Probability/probabilities of response. Multiple values can be
#' passed to it.
#' \itemize{
#' \item In \code{pae_simple()} and \code{pae_vol()} it defines response
#' probabilities for unsuccessful hunters.
#' \item In \code{pae_mand()} it defines
#' response probabilities for successful hunters, as they are the only ones
#' mandated to report.
#' }
#' @inheritParams pop
#' @inheritParams est
#'
#' @details
#' These functions provide an easy way to replicate multiple simulations within
#' a single function. Be aware that depending on how you define arguments, these
#' functions may take a long time to execute. Large populations, long vectors
#' for \code{resp} and/or \code{bias}, and completing many
#' simulations using \code{times} can all take a while
#' to execute.
#'
#' @return A tibble, containing an estimate of harvest for each level of
#' response rate, standard error, and absolute percent error, among
#' other important metadata.\cr\cr
#'
#' @seealso
#' \code{\link{pop}} and \code{\link{est}} to better understand the functions
#' that these are wrappers for.
#'
#' @examples
#' # Calculate harvest estimates from a population that was surveyed using a
#' # simple random sample:
#' dat <- pae_simple(
#'   n         = 1000,
#'   split     = 0.7,
#'   success1  = 0.25,
#'   success0  = 0.6,
#'   sample    = 0.5,
#'   resp      = seq(0.2, 0.8, 0.2),
#'   resp_bias = c(1, 1.1, 1.2),
#'   fus       = TRUE,
#'   fus_scale = 0.7,
#'   poststrat = FALSE,
#'   times     = 100
#' )
#'

NULL

# pae_mand() =================================================================
#' @rdname pae

pae_mand <- function(n, split = 1, success1, success0 = success1,
                     resp, times = 1) {

  dat <- pop_mand(n, split, success1, success0, resp, times)
  out <- est_mand(dat)

  return(out)
}

# pae_simple() ===============================================================
#' @rdname pae

pae_simple <- function(n, split = 1, success1, success0 = success1, sample,
                       resp, bias, fus = FALSE, fus_scale = NULL,
                       times = 1, poststrat = FALSE) {

  dat <- pop_simple(n,
                    split,
                    success1,
                    success0,
                    sample,
                    resp,
                    bias,
                    fus,
                    fus_scale,
                    times)

  out <- est_simple(dat)
  return(out)
}

# pae_vol() ==================================================================
#' @rdname pae

pae_vol <- function(n, split = 1, success1, success0 = success1, resp,
                    bias, fus = FALSE, fus_sample = NULL,
                    fus_scale = NULL, poststrat = FALSE, times){

  dat <- pop_vol(n,
                 split,
                 success1,
                 success0,
                 resp,
                 bias,
                 fus,
                 fus_sample,
                 fus_scale,
                 times)

  out <- est_vol(dat, poststrat)
  return(out)
}
