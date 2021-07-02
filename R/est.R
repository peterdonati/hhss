# Documentation ================================================================
#'
#' Estimate Total Harvest
#'
#' @description A generic function for estimating total harvest from objects
#' created by the \code{\link{survey}} simulation functions.
#'
#' @param simdat Survey simulation data. Must be an object
#' created from one of the three \code{\link{survey}} functions.
#' @details
#' In simulations with follow up surveys, two separate estimates are made.
#' One estimate from the respondents to
#' the initial survey and the other from respondents to follow up surveys.
#' In the case of estimates from \code{simple()} outputs,
#' the proportion of \code{init_resp/init_sample} is
#' used to scale the initial estimate (i.e. the prop. of total respondents
#' to total sampled is assumed to make up the same
#' prop. of the entire population). Then, the follow up respondents are assumed
#' to be representative of the remaining proportion of the entire population.
#' This remaining proportion is used to scale the follow up estimate.
#' Both scaled estimates are then added together to create the combined
#' total population estimate.\cr
#'
#' This is similar to estimates made from \code{census()} outputs,
#' except in that case the initial scaling proportion is \code{init_resp/N}.
#'
#' Estimates for \code{mand()} outputs assume 100\% reporting by successful
#' hunters for initial reports if there is no follow up. If a follow up survey
#' was simulated, it creates a harvest estimate from the follow up sample to
#' estimate total harvest for \emph{the non-reporting portion of
#' the population only}, and then adds that to the sum of
#' initially reported harvests.\cr\cr
#'
#' @return A data frame, containing the following variables:
#' \itemize{
#' \item \code{N}: Hunter population size.
#' \item \code{resp_bias}: Bias of successful hunters to report,
#'     relative to unsuccessful hunters.
#' \item \code{resp_rate}: Underlying response probability, before any
#'     scaling/bias.
#' \item \code{mean_init_n}: Average total initial survey responses.
#' \item \code{mean_fol_n}: Average total follow-up survey responses.
#' \item \code{true_harvest}: True harvest of population.
#' \item \code{min_hvst_est}: Minimum harvest estimate of the survey
#'     repetitions (if any) at the response bias and response rate shown for that row.
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
#'
#' @examples
#' # First, make a population:
#' my_pop <- pop(N = 10000, split = 0.7, success1 = 0.25, success2 = 0.32)
#'
#' # Then simulate a survey for that population:
#' my_pop_simple_followup <- simple(
#'   x = my_pop,
#'   sample = 0.5,
#'   resp = c(0.4, 0.6),
#'   bias = seq(1, 1.3, 0.1),
#'   fol = TRUE,
#'   fol_scale = 0.7,
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

# census method ================================================================
#' @export
est.survsim_census <- function(simdat){

  simdat <- purrr::flatten(simdat)
  N <- extract_and_check(simdat, "N")
  thvst <- extract_and_check(simdat, "true_harvest")

  est_census <- function(pop_dat){

    init_n <- pop_dat$init_resp
    init_weight <- N / init_n

    init_sd <- short_sd(init_n, pop_dat$init_yes)
    init_SE <- (init_sd / sqrt(init_n)) * init_weight * init_n
    init_SE <- init_SE * sqrt((N - init_n) / (N - 1)) # fpc

    init_est <- as.integer(pop_dat$init_yes * init_weight) # shave decimals

    follow_check <- is_follow(simdat)
    if (follow_check == length(simdat)){

      if (pop_dat$fol_resp != 0) {

        fol_n <- pop_dat$fol_resp
        fol_weight <- N / fol_n

        fol_sd <- short_sd(fol_n, pop_dat$fol_yes)
        fol_SE <- (fol_sd / sqrt(fol_n)) * fol_weight * fol_n
        fol_SE <-  fol_SE * sqrt((N - fol_n) / (N - 1)) # fpc

        fol_est <- as.integer(pop_dat$fol_yes * fol_weight)

        # No init sample, so full proportion with N:
        init_prop <- init_n / N
        # Assume follow up represents the remainder:
        fol_prop <- 1 - init_prop

        combined_est <- (init_est * init_prop) + (fol_est * fol_prop)
        combined_est <- as.integer(combined_est)
        combined_SE <- (init_SE * init_prop) + (fol_SE * fol_prop)

        estout <- data.frame(
          resp_rate = pop_dat$init_uns_rate,
          resp_bias = pop_dat$resp_bias,
          init_n,
          fol_n,
          est_harvest = combined_est,
          est_SE = combined_SE,
          ARE = abs((combined_est - thvst) / thvst),
          sqer = ((combined_est - thvst)^2)
        )
      } else if (pop_dat$fol_resp == 0) {
        estout <- data.frame(
          resp_rate = pop_dat$init_uns_rate,
          resp_bias = pop_dat$resp_bias,
          init_n,
          fol_n = NA,
          est_harvest = init_est,
          est_SE = init_SE,
          ARE = abs((init_est - thvst) / thvst),
          sqer = ((init_est - thvst)^2)
        )
      } else {
        stop("Follow up estimate error")
      }

    } else if (follow_check == 0) {
      estout <- data.frame(
        resp_rate = pop_dat$init_uns_rate,
        resp_bias = pop_dat$resp_bias,
        init_n,
        fol_n = NA,
        est_harvest = init_est,
        est_SE = init_SE,
        ARE = abs((init_est - thvst) / thvst),
        sqer = ((init_est - thvst)^2)
      )
    } else {
      stop("Error with 'simdat' structure.")
    }

    return(estout)
  }

  # Use map() to pull individual simulations and make individual estimates:
  ests <- purrr::map_dfr(simdat, est_census)
  out <- output_summarizer(ests, N, thvst)
  return(out)
}

# mand method ==================================================================
#' @export
est.survsim_mand <- function(simdat){

  simdat <- purrr::flatten(simdat)
  N <- extract_and_check(simdat, "N")
  thvst <- extract_and_check(simdat, "true_harvest")

  est_mand <- function(pop_dat){

    # Only successful hunters respond:
    init_est <- pop_dat$init_resp

    follow_check <- is_follow(simdat)

    if (follow_check == length(simdat)){
      if (pop_dat$fol_resp != 0) {

        fol_n <- pop_dat$fol_resp
        # N for follow up is whoever didn't report initially because the
        # estimate is for the nonresponding portion of the population only!!:
        fol_N <- pop_dat$N - pop_dat$init_resp
        fol_weight <- fol_N / fol_n

        fol_sd <- short_sd(fol_n, pop_dat$fol_yes)
        fol_SE <- (fol_sd / sqrt(fol_n)) * fol_weight * fol_n
        fol_SE <- fol_SE * sqrt((fol_N - fol_n) / (fol_N - 1)) # fpc

        fol_est <- as.integer(pop_dat$fol_yes * fol_weight) # shave decimals

        combined_est <- init_est + fol_est

        estout <- data.frame(
          resp_rate = pop_dat$init_rate,
          resp_bias = pop_dat$resp_bias,
          init_n = pop_dat$init_resp,
          fol_n = pop_dat$fol_resp,
          est_harvest = combined_est,
          est_SE = fol_SE, # Assume no error for init.
          ARE = abs((combined_est - thvst) / thvst),
          sqer = ((combined_est - thvst)^2)
        )
      } else if (pop_dat$fol_resp == 0) {
        estout <- data.frame(
          resp_rate = pop_dat$init_rate,
          resp_bias = NA,
          init_n = init_est,
          fol_n = NA,
          est_harvest = init_est,
          est_SE = 0L, # Because assuming 100% reporting
          ARE = abs((init_est - thvst) / thvst),
          sqer = ((init_est - thvst)^2)
        )
      } else {stop("Follow up estimate error")}

    } else if (follow_check == 0) {
      estout <- data.frame(
        resp_rate = pop_dat$init_rate,
        resp_bias = NA,
        init_n = init_est,
        fol_n = NA,
        est_harvest = init_est,
        est_SE = 0L, # Because assuming 100% reporting
        ARE = abs((init_est - thvst) / thvst),
        sqer = ((init_est - thvst)^2)
      )
    } else {
      stop("Error with 'simdat' structure.")
    }

    return(estout)
  }

  # Use map() to pull individual dataframes from simdat list
  # and make individual estimates:
  ests <- purrr::map_dfr(simdat, est_mand)

  out <- output_summarizer(ests, N, thvst)
  out <- as.data.frame(out)
  return(out)
}

# simple method ================================================================
#' @export
est.survsim_simple <- function(simdat){

  simdat <- purrr::flatten(simdat)
  N <- extract_and_check(simdat, "N")
  thvst <- extract_and_check(simdat, "true_harvest")

  est_simp <- function(pop_dat){

    init_n <- pop_dat$init_resp
    init_weight <- N / init_n

    init_sd <- short_sd(init_n, pop_dat$init_yes)
    init_SE <- (init_sd / sqrt(init_n)) * init_weight * init_n
    init_SE <- init_SE * sqrt((N - init_n) / (N - 1)) # fpc

    init_est <- as.integer(pop_dat$init_yes * init_weight) # shave decimals

    follow_check <- is_follow(simdat)
    if (follow_check == length(simdat)){
      if (pop_dat$fol_resp != 0) {

        fol_n <- pop_dat$fol_resp
        fol_weight <- N / fol_n

        fol_sd <- short_sd(fol_n, pop_dat$fol_yes)
        fol_SE <- (fol_sd / sqrt(fol_n)) * fol_weight * fol_n
        fol_SE <- fol_SE * sqrt((N - fol_n) / (N - 1)) # fpc

        fol_est <- as.integer(pop_dat$fol_yes * fol_weight)

        # assume proportion of respondents to initial sample represents the
        # same proportion of entire population:
        init_prop <- pop_dat$init_resp / pop_dat$init_sample
        # assume fol respondents represent rest of the pop:
        fol_prop  <- 1 - init_prop

        combined_est <- (init_est * init_prop) + (fol_est * fol_prop)
        combined_est <- as.integer(combined_est)
        combined_SE <- (init_SE * init_prop) + (fol_SE * fol_prop)

        estout <- data.frame(
          resp_rate = pop_dat$init_uns_rate,
          resp_bias = pop_dat$resp_bias,
          init_n,
          fol_n,
          est_harvest = combined_est,
          est_SE = combined_SE,
          ARE = abs((combined_est - thvst) / thvst),
          sqer = ((combined_est - thvst)^2)
        )
      } else if (pop_dat$fol_resp == 0) {
        estout <- data.frame(
          resp_rate = pop_dat$init_uns_rate,
          resp_bias = pop_dat$resp_bias,
          init_n,
          fol_n = NA,
          est_harvest = init_est,
          est_SE = init_SE,
          ARE = abs((init_est - thvst) / thvst),
          sqer = ((init_est - thvst)^2)
        )
      } else {
        stop("Follow up estimate error")
      }
    } else if (follow_check == 0) {
      estout <- data.frame(
        resp_rate = pop_dat$init_uns_rate,
        resp_bias = pop_dat$resp_bias,
        init_n,
        fol_n = NA,
        est_harvest = init_est,
        est_SE = init_SE,
        ARE = abs((init_est - thvst) / thvst),
        sqer = ((init_est - thvst)^2)
      )
    } else {
      stop("Error with 'simdat' structure.")
    }

    return(estout)
  }

  # Use map() to pull individual simulations and make individual estimates:
  ests <- purrr::map_dfr(simdat, est_simp)

  out <- output_summarizer(ests, N, thvst)
  out <- as.data.frame(out)
  return(out)
}

# Helpers ======================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Groups individual harvest estimate outputs and reports findings. This creates
# a consistent output for all possible scenarios. Called by the est function.
output_summarizer <- function(ests, N, thvst){
  # Group, so avgs for each repetition of the same resp and bias can be made
  ests <- dplyr::group_by(ests, resp_bias, resp_rate)

  out <- dplyr::summarise(
    ests,
    N = N,
    mean_init_n = round(mean(init_n), 2),
    mean_fol_n = round(mean(fol_n), 2),
    true_harvest = thvst,
    min_hvst_est = min(est_harvest),
    max_hvst_est = max(est_harvest),
    mean_hvst_est = mean(est_harvest),
    mean_SE = mean(est_SE),
    MARE = mean(ARE),
    RRMSE = sqrt(mean(sqer)) / true_harvest,
    .groups = "drop"
  )

  var_order <- c(
    "N", "resp_bias", "resp_rate", "mean_init_n", "mean_fol_n", "true_harvest", "min_hvst_est",
    "max_hvst_est", "mean_hvst_est", "mean_SE", "MARE", "RRMSE"
  )
  out <- out[ , var_order]
  out <- as.data.frame(out)

  return(out)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Checks for single 'N' and 'true_harvest' within simdat.
extract_and_check <- function(x, check){

  y <- vector(mode = "integer", length = length(x))
  for (i in 1:length(x)) {
    y[i] <- x[[i]][[check]]
  }

  if (length(unique(y)) == 1) {
    return(y[[1]])
  } else {
    stop("Cannot make an estimate on multiple populations at once", call. = F)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Checks for follow up sims:
is_follow <- function(check_me) {
  sum(names(purrr::flatten(check_me)) %in% "fol_resp")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variation on std. dev shortcut formula. binary data only,
# so sum(observations^2) = sum(observations). 0^2 = 0 so also only need to know
# the sum of observations that were 1.
short_sd <- function(n, sum) {

  # To avoid integer overload for large populations:
  n <- as.numeric(n)
  sum <- as.numeric(sum)

  find_sqrt <- (n * sum - sum^2) / (n * (n - 1))
  found_it <- sqrt(find_sqrt)
  return(found_it)
}
