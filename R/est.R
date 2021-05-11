# Documentation ================================================================
#'
#' Estimate Total Harvest
#'
#' @description A generic function for estimating total harvest from objects
#' created by the \code{\link{survey}} simulation functions.
#'
#' @param simdat Survey data. Must be an object
#' created from either \code{mand()}, \code{simple()}, or \code{vol()}.
#'
#' @details
#' All estimates use \code{survey::svydesign()} and
#' \code{survey::svytotal()} to estimate total harvest.\cr
#'
#' Estimates are
#' calculated a bit different when a follow up survey is simulated in the
#' data. In that case, two separate estimates are made using
#' \code{survey::svytotal()}. One estimate from the respondents to
#' the initial survey and the other from respondents
#' to follow up surveys.
#' In the case of estimates from \code{simple()} outputs,
#' the proportion of (hunters initially responding / hunters sampled) is
#' used to scale the initial estimate (i.e. the prop. of respondents is
#' assumed to make up the same
#' prop. of the entire population). Then, the follow up respondents are assumed
#' to be representative of the remaining proportion of the entire population.
#' This proportion is used to scale the follow up estimate.
#' Both scaled estimates are then added together to create the combined
#' estimate.\cr
#'
#' This is similar to estimates made from \code{vol()} outputs,
#' except in that case, the proportion of initial respondents \emph{is} the
#' proportion of the population that responded.\cr
#'
#' Estimates for \code{mand()} outputs assume 100\% reporting by successful
#' hunters for initial reports if there is no follow up. If a follow up survey
#' was simulated, it creates a harvest estimate from the follow up sample to
#' estimate total harvest for the non-reporting portion of the population, and
#' then adds that to the sum of initially reported harvests.\cr\cr
#'
#' @return A tibble, containing the following variables:
#' \itemize{
#' \item \code{pop_size}: Hunter population size.
#' \item \code{resp_bias}: Bias of successful hunters to report,
#' relative to unsuccessful hunters.
#' \item \code{resp_rate}: Underlying response probabilities of hunters, if
#' there was no bias.
#' \item \code{true_hvst}: True harvest of population.
#' \item \code{mean_n}: Average total responses. Includes both initial
#' and follow-up responses.
#' \item \code{min_hvst_est}: Minimum harvest estimate of the survey
#' repetitions at the response bias and response rate shown for that row.
#' \item \code{max_hvst_est}: Maximum harvest estimate.
#' \item \code{mean_hvst_est}: Average harvest estimate.
#' \item \code{mean_SE}: Average standard error across repetitions.
#' \item \code{MARE}: Mean absolute relative error.
#' \item \code{RRMSE}: Relative root mean squared error.
#' }
#'
#' @seealso
#' \code{\link{pop}} and \code{\link{survey}} for how to create the data to be
#' input to this function.\cr\cr
#' \code{\link[survey]{svydesign}}, \code{\link[survey]{surveysummary}} for
#' details on how estimates are calculated.
#'
#' @examples
#' # First, make a population:
#' my_pop <- pop(N = 10000, split = 0.7, success1 = 0.25, success0 = 0.32)
#'
#' # Then simulate a survey for that population:
#' my_pop_simple_followup <- simple(
#'   x = my_pop,
#'   sample = 0.5,
#'   resp = c(0.4, 0.6),
#'   bias = seq(1, 1.3, 0.1),
#'   fus = TRUE,
#'   fus_scale = 0.7,
#'   times = 10
#' )
#'
#' # Finally, make your estimates:
#' est(my_pop_simple_followup)


# est() ========================================================================
#' @export
est <- function(simdat){
  UseMethod("est")
}

# mand method ==================================================================
#' @export
est.survsim_mand <- function(simdat){

  simdat <- purrr::flatten(simdat)
  N <- simdat[[1]]$pop_size[[1]]
  thvst <- simdat[[1]]$true_harvest[[1]]

  if ("fus_resp" %in% names(simdat[[1]])){
    est_mand <- function(pop_dat){

      # Initial estimate will just be sum of reports:
      init_est <- sum(pop_dat$init_resp)

      # Separate estimate of non-respondents:
      fus_resp_only <- subset(pop_dat, fus_resp == 1)

      fus_resp_only$fpc <- N - sum(pop_dat$init_resp)

      fus_design <- survey::svydesign(
        ids = ~1,
        probs = nrow(fus_resp_only) / (N - sum(pop_dat$init_resp)),
        data = fus_resp_only,
        fpc = ~fpc
      )

      fus_est <- survey::svytotal(~harvest, fus_design)

      combined_est <- init_est + fus_est

      estout <- tibble::tibble(
        resp_rate = pop_dat$resp_rate[[1]],
        resp_bias = pop_dat$resp_bias[[1]],
        true_harvest = thvst,
        n = sum(init_est, nrow(fus_resp_only)),
        est_harvest = as.vector(combined_est),
        est_SE = as.vector(survey::SE(fus_est)), # Assume no error for init.
        ARE = abs((est_harvest - true_harvest) / true_harvest),
        sqer = ((est_harvest - true_harvest)^2)
      )
      return(estout)
    }
  } else {

    # If there was not a follow up, simply report the sum of init:
    est_mand <- function(pop_dat){

      estout <- tibble::tibble(
        resp_rate = pop_dat$resp_rate[[1]],
        resp_bias = NA_real_,
        true_harvest = pop_dat$true_harvest[[1]],
        n = sum(pop_dat$init_resp, na.rm = TRUE),
        est_harvest = n,
        est_SE = 0L, # Because assuming 100% reporting
        ARE = abs((est_harvest - true_harvest) / true_harvest),
        sqer = ((est_harvest - true_harvest)^2)
      )

      return(estout)
    }
  }

  # Use map() to pull individual dataframes from simdat list
  # and make individual estimates:
  ests <- purrr::map_dfr(simdat, est_mand)

  out <- output_summarizer(ests = ests, N = N)
  out <- as.data.frame(out)
  return(out)
}

# simple method ================================================================
#' @export
est.survsim_simple <- function(simdat){

  simdat <- purrr::flatten(simdat)
  N <- simdat[[1]]$pop_size[[1]]
  thvst <- simdat[[1]]$true_harvest[[1]]

  est_simp <- function(pop_dat){
    init_resp_only <- dplyr::filter(pop_dat, init_resp == 1)
    init_design <- survey::svydesign(
      ids = ~1,
      probs = nrow(init_resp_only) / N,
      data = init_resp_only,
      fpc = ~pop_size
    )
    init_est <- survey::svytotal(~harvest, init_design)

    if ("fus_resp" %in% names(pop_dat)){

      # If resp_rate is not less than 1, then there was nobody to follow
      # up with, so ignore this next step:
      if (all(pop_dat$uns_resp_rate < 1) | all(pop_dat$suc_resp_rate < 1)){

        fus_resp_only <- dplyr::filter(pop_dat, fus_resp == 1)

        fus_design <- survey::svydesign(
          ids = ~1,
          probs = nrow(fus_resp_only) / N,
          data = fus_resp_only,
          fpc = ~pop_size
        )

        fus_est <- survey::svytotal(~harvest, fus_design)

        # assume proportion of respondents to initial sample represents the
        # same proportion of entire population:
        init_prop <- sum(pop_dat$init_resp, na.rm = TRUE) /
          sum(pop_dat$sample)
        # assume fus respondents represent rest of the pop:
        fus_prop  <- 1 - init_prop

        combined_est <- (init_est * init_prop) + (fus_est * fus_prop)

        combined_SE <- (init_prop * survey::SE(init_est)) +
          (fus_prop * survey::SE(fus_est))

        estout <- tibble::tibble(
          resp_rate = pop_dat$uns_resp_rate[[1]],
          resp_bias = pop_dat$resp_bias[[1]],
          true_harvest = thvst,
          n = sum(pop_dat$init_resp, pop_dat$fus_resp, na.rm = TRUE),
          est_harvest = as.vector(combined_est),
          est_SE = as.vector(combined_SE),
          ARE = abs((est_harvest - true_harvest) / true_harvest),
          sqer = ((est_harvest - true_harvest)^2)
        )

      } else if (all(pop_dat$uns_resp_rate == 1) &
                 all(pop_dat$suc_resp_rate == 1)){
        # If follow up survey was planned, but everyone responded initially:
        estout <- tibble::tibble(
          resp_rate = pop_dat$uns_resp_rate[[1]],
          resp_bias = pop_dat$resp_bias[[1]],
          true_harvest = thvst,
          n = sum(pop_dat$init_resp, na.rm = T),
          est_harvest = as.vector(init_est),
          est_SE = as.vector(survey::SE(init_est)),
          ARE = abs((est_harvest - true_harvest) / true_harvest),
          sqer = ((est_harvest - true_harvest)^2)
        )

      } else {
        stop ("Conflicting response rates in a single population",
              call. = FALSE)
      }

    } else {
      # If there was no follow up survey simulated:
      estout <- tibble::tibble(
        resp_rate = pop_dat$uns_resp_rate[[1]],
        resp_bias = pop_dat$resp_bias[[1]],
        true_harvest = thvst,
        n = sum(pop_dat$init_resp, na.rm = T),
        est_harvest = as.vector(init_est),
        est_SE = as.vector(survey::SE(init_est)),
        ARE = abs((est_harvest - true_harvest) / true_harvest),
        sqer = ((est_harvest - true_harvest)^2)
      )
    }
    return(estout)
  }

  # Use map() to pull individual simulations and make individual estimates:
  ests <- purrr::map_dfr(simdat, est_simp)

  out <- output_summarizer(ests = ests, N = N)
  out <- as.data.frame(out)
  return(out)
}

# vol method ===================================================================
#' @export
est.survsim_vol <- function(simdat){

  simdat <- purrr::flatten(simdat)
  N <- simdat[[1]]$pop_size[[1]]
  thvst <- simdat[[1]]$true_harvest[[1]]

  est_vol <- function(pop_dat){
    init_resp_only <- dplyr::filter(pop_dat, init_resp == 1)
    init_design <- survey::svydesign(
      ids = ~1,
      probs = nrow(init_resp_only) / N,
      data = init_resp_only,
      fpc = ~pop_size
    )
    init_est <- survey::svytotal(~harvest, init_design)

    if ("fus_resp" %in% names(pop_dat)){
      # if response rate was already 1 for everyone, there is nobody to
      # follow up with,so skip the following step:
      if (all(pop_dat$uns_resp_rate < 1) | all(pop_dat$suc_resp_rate < 1)){

        fus_resp_only <- dplyr::filter(pop_dat, fus_resp == 1)
        fus_design <- survey::svydesign(ids = ~1,
                                        probs = nrow(fus_resp_only) / N,
                                        data = fus_resp_only,
                                        fpc = ~pop_size)

        fus_est <- survey::svytotal(~harvest, fus_design)

        # The prop. of respondents to self-report IS the proportion
        # of the population who reported. Can just take the mean because
        # init_resp is binary:
        init_prop <- mean(pop_dat$init_resp, na.rm = TRUE)

        # assume fus respondents reflect the rest of the pop:
        fus_prop  <- 1 - init_prop
        combined_est <- (init_est * init_prop) + (fus_est * fus_prop)

        combined_SE <- (init_prop * survey::SE(init_est)) +
          (fus_prop * survey::SE(fus_est))

        estout <- tibble::tibble(
          resp_rate = pop_dat$uns_resp_rate[[1]],
          resp_bias = pop_dat$resp_bias[[1]],
          true_harvest = thvst,
          n = sum(pop_dat$init_resp, pop_dat$fus_resp, na.rm = T),
          est_harvest = as.vector(combined_est),
          est_SE = as.vector(combined_SE),
          ARE = abs((est_harvest - true_harvest) / true_harvest),
          sqer = ((est_harvest - true_harvest)^2)
        )

      } else if (all(pop_dat$uns_resp_rate == 1) &
                 all(pop_dat$suc_resp_rate == 1)){
        # if everyone had an initial response rate of 1, then the entire
        # population responded and there was nobody to follow up with:
        estout <- tibble::tibble(
          resp_rate = pop_dat$uns_resp_rate[[1]],
          resp_bias = pop_dat$resp_bias[[1]],
          true_harvest = thvst,
          n = sum(pop_dat$init_resp, na.rm = T),
          est_harvest = as.vector(init_est),
          est_SE = as.vector(survey::SE(init_est)),
          ARE = abs((est_harvest - true_harvest) / true_harvest),
          sqer = ((est_harvest - true_harvest)^2)
        )
      } else {
        stop ("Conflicting response rates in a single population",
              call. = FALSE)
      }

    } else {
      # If there was no follow up survey simulated:
      estout <- tibble::tibble(
        resp_rate = pop_dat$uns_resp_rate[[1]],
        resp_bias = pop_dat$resp_bias[[1]],
        true_harvest = thvst,
        n = sum(pop_dat$init_resp, na.rm = T),
        est_harvest = as.vector(init_est),
        est_SE = as.vector(survey::SE(init_est)),
        ARE = abs((est_harvest - true_harvest) / true_harvest),
        sqer = ((est_harvest - true_harvest)^2)
      )
    }
    return(estout)
  }

  # Use map() to pull individual simulations and make individual estimates:
  ests <- purrr::map_dfr(simdat, est_vol)
  out <- output_summarizer(ests = ests, N = N)
  return(out)
}

# Helpers ======================================================================

# output_summarizer() ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Groups individual harvest estimate outputs and reports findings. This creates
# a consistent output for all possible scenarios. Called by the est function.
output_summarizer <- function(ests, N){
  # Group, so avgs for each repetition of the same resp and bias can be made
  ests <- dplyr::group_by(ests, resp_bias, resp_rate)

  out <- dplyr::summarise(
    ests,
    pop_size = N,
    true_hvst = mean(true_harvest),
    mean_n = mean(n),
    min_hvst_est = min(est_harvest),
    max_hvst_est = max(est_harvest),
    mean_hvst_est = mean(est_harvest),
    mean_SE = mean(est_SE),
    MARE = mean(ARE),
    RRMSE = sqrt(mean(sqer)) / true_hvst,
    .groups = "drop"
  )

  out <- dplyr::select(
    out, pop_size, resp_bias, resp_rate, tidyselect::everything()
  )

  return(out)
}
