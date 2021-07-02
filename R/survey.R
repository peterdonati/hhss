# Documentation ================================================================
#' Survey Simulations
#'
#' @name survey
#'
#' @aliases mand simple census
#'
#' @description
#' The survey functions take an output from \code{\link{pop}} and simulate
#' survey responses based on the method specified.
#' \itemize{
#'     \item \code{census()} creates a simulation where
#'         \strong{\emph{all hunters report, successful or not.}}
#'         This can mimic both mandatory reporting for all hunters
#'         as well as voluntary reporting.
#'         Follow up surveys can be completed on the non-responding portion
#'         of the population by simple random sampling.
#'     \item \code{mand()} creates a simulation where \strong{\emph{only
#'         successful hunters report}} and a follow up sample of
#'         non-respondents can be taken through a simple random sample.
#'     \item \code{simple()} creates a simulation where a population of hunters
#'         are surveyed using a simple random sample. Follow up survey is
#'         completed by following up with all non-respondents from the
#'         initial sample.
#' }
#'
#' @details
#' More than one value can be supplied to \code{resp} and \code{bias}. These
#' functions automatically create a full factorial design on
#' these two arguments.
#' \cr
#'
#' If any scaling arguments scale probabilities to be > 1, the
#' probabilities will silently be limited to 1.
#'
#' @param x An output from \code{\link{pop}}. It is the population to
#'     simulate response for.
#' @param sample Probability a hunter is sampled for a survey
#' @param resp Probability/probabilities of response.
#'     \itemize{
#'         \item In \code{simple()} and \code{census()} it defines response
#'             probabilities for unsuccessful hunters.
#'         \item In \code{mand()} it defines response probabilities for initial
#'             reporting, and then response probabilities for unsuccessful
#'             hunters in follow up samples.
#'     }
#' @param bias Scales the value(s) supplied to \code{resp} to create response
#'     probabilities for successful hunters.
#'     Introduces response bias for any value not equal to 1.
#' @param fol Logical. If \code{TRUE}, a single follow up survey will be
#'     simulated.
#' @param fol_scale Scales initial response probabilities,
#'     creating new probabilities of response for follow up surveys.
#' @param fol_sample Probability that a non-respondent is sampled for a follow
#'     up survey.
#' @param times The number of times to repeat the simulation.
#'
#' @return A list of class \code{survsim_census}, \code{survsim_mand},
#' or \code{survsim_simple} where the length is equal to the
#' integer supplied to \code{times}. The ultimate elements are data frames
#' that will contain some,
#' but not all, of these variables:
#' \itemize{
#' \item \code{N}: The population size.
#' \item \code{true_harvest}: The sum of harvests from the population.
#' \item \code{resp_bias}: The response bias simulated.
#' \item \code{init_rate}: The response probability for hunters to
#'     initially report harvest. Only reported in \code{mand()} outputs.
#' \item \code{init_uns_rate}: The probability at which a hunter responded
#'     to an initial survey if they were unsuccessful in harvesting.
#' \item \code{init_suc_rate}: The probability at which a hunter responded
#'     to an initial survey if they were successful in harvesting.
#' \item \code{init_sample}: The sum of hunters sampled in initial survey.
#' \item \code{init_resp}: The sum of responses to intial survey.
#' \item \code{init_yes}: The sum of initial responses that were from hunters
#'     who harvested.
#' \item \code{init_no}: The sum of initial responses that were from hunters
#'     who did not harvest.
#' \item \code{fol_sample}: The sum of hunters sampled for follow up.
#' \item \code{fol_uns_rate}: The probability at which a hunter
#'     responded to a follow up survey if they did not harvest.
#' \item \code{fol_suc_rate}: The probability at which a hunter
#'     responded to a follow up survey if they harvested.
#' \item \code{fol_resp}: The sum of hunters that responded to the follow up.
#' \item \code{fol_yes}: The sum of follow up responses that were from hunters
#'     who harvested.
#' \item \code{fol_no}: The sum of follow up responses that were from hunters
#'     who did not harvest.
#' }
#'
#' @examples
#' # First, create a population:
#' my_pop <- pop(N = 1000, split = 0.7, success1 = 0.25, success2 = 0.6)
#'
#' # Simulate a simple random sample from that population:
#' simple(
#'   my_pop,
#'   sample = 0.4,
#'   resp = 0.3,
#'   bias = 1,
#'   times = 10
#' )
#'
#' # Multiple values can be passed to 'resp' and 'bias' arguments to create
#' # simulations for each unique pairing of the two:
#' census(
#'   my_pop,
#'   resp = seq(0.3, 0.8, 0.1),
#'   bias = c(1, 1.1, 1.2),
#'   fol = TRUE,
#'   fol_sample = 0.4,
#'   fol_scale = 0.7,
#'   times = 10
#' )
#'

# census() =====================================================================
#' @rdname survey
#' @export

census <- function(x, resp, bias, fol = FALSE,
                   fol_sample = NULL, fol_scale = NULL, times = 1) {

  #Argument checks ----
  if (!inherits(x, "hhss_pop")){
    stop ("'x' not of class 'hhss_pop'")
  }

  if (times %% 1 != 0){
    stop ("'times' must be a whole number.", call. = FALSE)
  }

  argcheck <- c(fol_sample, resp)
  if (any(argcheck > 1) || any(argcheck <= 0)){
    stop (
      "'resp' and 'fol_sample' must be proportions and > 0.",
      call. = FALSE
    )
  }

  if (fol && (missing(fol_scale) || missing(fol_sample))){
    stop (
      "If 'fol' = TRUE, 'fol_scale' and 'fol_sample' arguments
      must be defined.",
      call. = FALSE
    )
  }

  if (!fol && (!missing(fol_scale) || !missing(fol_sample))){
    stop ("'fol_scale' and/or 'fol_sample' are defined, but 'fol' = FALSE.",
          call. = FALSE)
  }

  if (!missing(fol_scale) && fol_scale > 1){
    message(
      "fol_scale > 1; Hunters more likely to respond to follow up
        than to initially report"
    )
  }

  if (any(bias < 1)) {
    message(
      "At least 1 value of 'bias' < 1; successful hunters will be
      less likely to respond to survey than unsuccessful hunters."
    )
  }

  # Actual function ----
  x <- as.data.frame(x)
  x <- pop_summarizer(x)

  single_sim_c <- function(.r, .b, dat){

    init_suc_rate <- changeto1(.r * .b) # scale for successful hunter resp bias
    init_yes <- rbinom(1, dat$N_success, init_suc_rate) # All can respond

    init_no <- rbinom(1, dat$N_unsuccess, .r)

    ss_out <- data.frame(
      N = dat$N,
      true_harvest = dat$N_success,
      resp_bias = .b,
      init_uns_rate = .r,
      init_suc_rate,
      init_resp = init_yes + init_no,
      init_yes,
      init_no
    )

    if (fol) {

      fol_samp_suc <- rbinom(1, dat$N_success - ss_out$init_yes, fol_sample)
      fol_suc_rate <- changeto1(init_suc_rate * fol_scale)
      fol_yes <- rbinom(1, fol_samp_suc, fol_suc_rate)

      # No need to remove initial respondents from unsuccessful portion in
      # follow up sample, because no initial respondents were unsuccessful:
      fol_samp_uns <- rbinom(1, dat$N_unsuccess - ss_out$init_no, fol_sample)
      fol_uns_rate <- changeto1(.r * fol_scale)
      fol_no <- rbinom(1, fol_samp_uns, fol_uns_rate)

      ss_out <- dplyr::mutate(
        ss_out,
        fol_sample = sum(fol_samp_suc, fol_samp_uns),
        fol_uns_rate,
        fol_suc_rate,
        fol_resp = sum(fol_yes, fol_no),
        fol_yes,
        fol_no
      )
    }

    return(ss_out)
  }

  out <- vector(mode = "list", length = times)
  for(i in 1:times){
    out[[i]] <- multi_sim(resp, bias, single_sim_c, x)
  }

  names(out) <- paste("Sim", 1:length(out))
  out <- survsim_census_class(out)
  return(out)
}


# mand() =======================================================================
#' @rdname survey
#' @export

mand <- function(x, resp, fol = FALSE, bias = NULL,
                 fol_sample = NULL, fol_scale = NULL, times = 1){

  # Argument checks ----
  if (!inherits(x, "hhss_pop")){
    stop ("'x' not of class 'hhss_pop'")
  }

  if (times %% 1 != 0){
    stop ("'times' must be a whole number.", call. = FALSE)
  }

  argcheck <- c(resp, fol_sample)
  if (any(argcheck > 1) || any(argcheck <= 0)) {
    stop("'resp' and/or 'fol_sample' must be proportions and > 0.",
         call. = FALSE)
  }

  if (fol){
    if (missing(fol_scale)) {
      stop("If 'fol' = TRUE, 'fol_scale' argument must be defined.",
           call. = FALSE)
    }
    if (missing(fol_sample)) {
      stop("If 'fol' = TRUE, 'fol_sample' argument must be defined.",
           call. = FALSE)
    }
    if (missing(bias)) {
      stop("If 'fol' = TRUE, 'bias' argument must be defined.",
           call. = FALSE)
    }
  }

  if (!fol){
    if (!missing(fol_scale)) {
      stop("'fol_scale' is defined, but 'fol' = FALSE.", call. = FALSE)
    }
    if (!missing(fol_sample)) {
      stop("'fol_sample' is defined, but 'fol' = FALSE.", call. = FALSE)
    }
    if (!missing(bias)) {
      stop(
        "'bias' is defined, but 'fol' = FALSE.
        Response bias is only simulated in follow up survey.",
        call. = FALSE
      )
    }
  }

  if (!missing(fol_scale) && fol_scale > 1) {
    message(
      "fol_scale > 1; Hunters more likely to respond to follow up
        than to initial survey."
    )
  }

  # Actual function ----
  x <- as.data.frame(x)
  x <- pop_summarizer(x)

  if (!fol){

    single_sim_m1 <- function(.r, dat){
      ss_out_nf <- data.frame(
        N = dat$N,
        true_harvest = dat$N_success,
        init_rate = .r,
        init_resp = rbinom(1, dat$N_success, .r) # Only successfuls report.
      )

      return(ss_out_nf)
    }

    # This scenario needs its own multi_sim function because there is no
    # reporting bias:
    multi_sim_m1 <- function(){
      ms_out <- purrr::map(resp, single_sim_m1, x)

      names(ms_out) <- paste0("resp ", resp, "; bias NA")
      return(ms_out)
    }

    out <- vector(mode = "list", length = times)
    for(i in 1:times){
      out[[i]] <- multi_sim_m1()
    }

    names(out) <- paste("Sim", 1:length(out))
    out <- survsim_mand_class(out)

    return(out)

  } else if (fol) {

    single_sim_m2 <- function(.r, .b, dat){
      ss_out_fol <- data.frame(
        N = dat$N,
        true_harvest = dat$N_success,
        resp_bias = .b,
        init_rate = .r,
        init_resp = rbinom(1, dat$N_success, .r) # Only harvests report
      )

      # Remove those who already responded initially from fol_samp_suc:
      fol_samp_suc <- rbinom(
        1, dat$N_success - ss_out_fol$init_resp, fol_sample
      )
      fol_suc_rate <- changeto1(.r * .b * fol_scale)
      fol_yes <- rbinom(1, fol_samp_suc, fol_suc_rate)

      # No need to remove initial respondents from unsuccessful portion in
      # follow up sample, because no initial respondents were unsuccessful:
      fol_samp_uns <- rbinom(1, dat$N_unsuccess, fol_sample)
      fol_uns_rate <- changeto1(.r * fol_scale)
      fol_no <- rbinom(1, fol_samp_uns, fol_uns_rate)

      ss_out_fol <- dplyr::mutate(
        ss_out_fol,
        fol_sample = sum(fol_samp_suc, fol_samp_uns),
        fol_uns_rate,
        fol_suc_rate,
        fol_resp = sum(fol_yes, fol_no),
        fol_yes,
        fol_no
      )

      return(ss_out_fol)
    }

    out <- vector(mode = "list", length = times)
    for(i in 1:times){
      out[[i]] <- multi_sim(resp, bias, single_sim_m2, x)
    }
    names(out) <- paste("Sim", 1:length(out))
    out <- survsim_mand_class(out)
    return(out)
  }
}

# simple() =====================================================================
#' @rdname survey
#' @export

simple <- function(x, sample, resp, bias,
                   fol = FALSE, fol_scale = NULL, times = 1) {

  # Argument checks ----
  if (!inherits(x, "hhss_pop")){
    stop ("'x' not of class 'hhss_pop'")
  }

  if (times %% 1 != 0){
    stop ("'times' must be a whole number.", call. = FALSE)
  }

  argcheck <- c(sample, resp)

  if (any(argcheck > 1) || any(argcheck <= 0)) {
    stop ("'sample' and 'resp' must only contain probabilities and be > 0.",
          call. = FALSE
    )
  }

  if (fol && missing(fol_scale)) {
    stop ("If 'fol' = TRUE, 'fol_scale' argument must be defined.",
          call. = FALSE)
  }

  if (!fol && !missing(fol_scale)) {
    stop ("'fol_scale' is defined, but 'fol' = FALSE.",
          call. = FALSE)
  }

  if (!missing(fol_scale) && fol_scale > 1) {
    message(
      "fol_scale > 1; Hunters more likely to respond to follow up
        than to initial survey."
    )
  }

  if (any(bias < 1)) {
    message(
      "At least 1 value of 'bias' < 1; successful hunters will be less
      likely to respond to survey than unsuccessful hunters."
    )
  }

  # Actual function ----
  x <- as.data.frame(x)
  x <- pop_summarizer(x)

  single_sim_s <- function(.r, .b, dat){

    init_samp_suc <- rbinom(1, dat$N_success, sample)
    init_suc_rate <- changeto1(.r * .b) # scale for successful hunter resp bias
    init_yes <- rbinom(1, init_samp_suc, init_suc_rate)

    init_samp_uns <- rbinom(1, dat$N_unsuccess, sample)
    init_no <- rbinom(1, init_samp_uns, .r) # unsuccessful hunters use base resp

    ss_out <- data.frame(
      N = dat$N,
      true_harvest = dat$N_success,
      resp_bias = .b,
      init_uns_rate = .r,
      init_suc_rate,
      init_sample = init_samp_suc + init_samp_uns,
      init_resp = init_yes + init_no,
      init_yes,
      init_no
    )

    if (fol) {

      fol_suc_rate <- changeto1(init_suc_rate * fol_scale)
      # Follow up with EVERY (successful) non-respondent:
      fol_yes <- rbinom(1, init_samp_suc - init_yes, fol_suc_rate)

      fol_uns_rate <- changeto1(.r * fol_scale)
      fol_no <- rbinom(1, init_samp_uns - init_no, fol_uns_rate)

      ss_out <- dplyr::mutate(
        ss_out,
        fol_uns_rate,
        fol_suc_rate,
        fol_resp = fol_yes + fol_no,
        fol_yes,
        fol_no
      )
    }

    return(ss_out)
  }

  out <- vector(mode = "list", length = times)
  for(i in 1:times){
    out[[i]] <- multi_sim(resp, bias, single_sim_s, x)
  }

  names(out) <- paste("Sim", 1:length(out))
  out <- survsim_simple_class(out)
  return(out)
}

# Helpers ======================================================================

# pop_summarizer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summarizes populations at beginning of each function to minimize memory usage
# during repetitions.

pop_summarizer <- function(pop){
  out <- data.frame(
    N = nrow(pop),
    N_success = sum(pop$harvest),
    N_unsuccess = nrow(pop) - sum(pop$harvest)
  )

  return(out)
}

# changeto1() ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To silently change response probabilities to 1 if scaling arguments scale it
# above 1. Called by survey functions.

changeto1 <- function(x) {
  ifelse(x > 1, 1, x)
}

# multi_sim() ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This function calls a function, 'f', that is a specific single_sim_*()
# function. Each single_sim_* function iteration completes a simulation for
# a single response and bias. the multi_sim() function here calls the
# single_sim_* function MULTIPLE TIMES, once for each unique pairing of
# response rate and bias.

multi_sim <- function(r, b, f, x){

  combos <- expand.grid(r = r, b = b)
  resp <- combos$r
  bias <- combos$b

  sim_list <- purrr::map2(resp, bias, f, x)

  names(sim_list) <- paste0("resp ", resp, "; bias ", bias)
  return(sim_list)
}

# survsim_* classes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
survsim_mand_class <- function(x){
  class(x) <- "survsim_mand"
  return(x)
}

survsim_simple_class <- function(x){
  class(x) <- "survsim_simple"
  return(x)
}

survsim_census_class <- function(x){
  class(x) <- "survsim_census"
  return(x)
}
