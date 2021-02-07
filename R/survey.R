#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate case_when
NULL

# Documentation ================================================================
#' Survey simulations
#'
#' @name survey
#'
#' @aliases mand simple vol
#'
#' @description
#' The survey functions take an output from \code{\link{pop}} and simulate
#' survey responses based on the method specified.
#' \itemize{
#'     \item \code{mand()} creates a simulation where only
#'         successful hunters are mandated to report and a follow up sample of
#'         non-reporters can be taken through a simple random sample.
#'     \item \code{simple()} creates a simulation where a population of hunters
#'         are surveyed using a simple random sample and a follow up survey of
#'         non-respondents from the original sample pool can be taken.
#'     \item \code{vol()} creates a simulation where reporting is voluntary,
#'         and both successful and unsuccessful hunters can report.
#'         Follow up surveys are completed on the non-responding portion of the
#'         population by simple random sampling.
#' }
#'
#' @details
#' More than one value can be supplied to \code{resp} and \code{bias}. A new
#' simulation will be completed for each unique pairing of the values passed
#' to those arguments.
#' \cr\cr
#' If any scaling arguments scale probabilities to be > 1, the
#' probabilities will silently be changed to 1.
#'
#' @param x An output from \code{pop}. The population to simulate response for.
#' @param sample Probability a hunter is sampled for a survey
#' @param resp Probability/probabilities of response.
#' \itemize{
#' \item In \code{simple()} and \code{vol()} it defines response
#' probabilities for unsuccessful hunters.
#' \item In \code{mand()} it defines response probabilities for initial
#' reporting, and then response probabilities for unsuccessful
#' hunters in follow up samples.
#' }
#' @param bias Scales the rate(s) of response for successful hunters, relative
#' to unsuccessful hunters. Introduces response bias for any value not equal to
#' 1.
#' @param fus Logical. If \code{TRUE}, a single follow up survey will be
#' simulated.
#' @param fus_scale Scales initial response probabilities,
#' creating new probabilities of response for follow up surveys.
#' @param fus_sample Probability that a non-respondent is sampled for a follow
#' up survey.
#' @param times The number of times to repeat the simulation.
#'
#' @return A multi-level list, where the length is equal to the integer supplied
#' to \code{times}. The ultimate elements are tibbles where each row represents
#' a hunter. A single tibble will contain some, but not all, of these variables:
#' \itemize{
#' \item \code{method}: The survey method that was used to gather responses.
#' \item \code{pop_size}: The population size.
#' \item \code{true_harvest}: The sum of harvests from the population.
#' \item \code{group}: The group in which the hunter was placed.
#' \item \code{harvest}: 1 for a successful hunter, and 0 if unsuccessful.
#' \item \code{sample}: 1 if the hunter was asked to participate in the
#' initial survey, 0 otherwise.
#' \item \code{resp_bias}: The response bias currently being simulated.
#' \item \code{resp_rate}: Only reported in \code{mand()} outputs.
#' It is the response probability for successful hunters to initially report.
#' \item \code{uns_resp_rate}: The probability at which a hunter will respond
#' to an initial survey if they were unsuccessful.
#' \item \code{suc_resp_rate}: The probability at which a hunter will respond
#' to an initial survey if they were successful.
#' \item \code{init_resp}: 1 if the hunter responded to the initial survey,
#' 0 otherwise.
#' \item \code{fus_uns_resp_rate}: The probability at which a hunter will
#' respond to a follow up survey if they were unsuccessful.
#' \item \code{fus_suc_resp_rate}: The probability at which a hunter will
#' respond to a follow up survey if they were successful.
#' \item \code{fus_sample}: 1 if the hunter was asked to participate in a
#' follow up survey, 0 otherwise.
#' \item \code{fus_resp}: 1 if they responded to the follow up survey,
#' 0 otherwise.
#' }
#'
#' @examples
#' # First, create a population:
#' my_pop <- pop(N = 1000, split = 0.7, success1 = 0.25, success0 = 0.6)
#'
#' # Simulate a simple random sample from that population:
#' simple(
#'   my_pop,
#'   sample = 0.4,
#'   resp = 0.3,
#'   bias = 1,
#'   times = 10
#'   )
#'
#' # Multiple values can be passed to resp and bias:
#' vol(
#'   my_pop,
#'   resp = seq(0.3, 0.8, 0.1),
#'   bias = c(1, 1.1, 1.2),
#'   fus = TRUE,
#'   fus_scale = 0.7,
#'   fus_sample = 0.4,
#'   times = 10
#'   )
#'
# mand() =======================================================================
#' @rdname survey
#' @export

mand <- function(x, resp, fus = FALSE, bias = NULL,
                 fus_sample = NULL, fus_scale = NULL, times = 1){

  # Argument checks ----
  if (times %% 1 != 0){
    stop ("'times' must be a whole number.", call. = FALSE)
  }

  argcheck <- c(resp, fus_sample)
  if (any(argcheck > 1) | any(argcheck <= 0)) {
    stop("'resp' and/or 'fus_sample' must be proportions and > 0.",
         call. = FALSE)
  }

  if (fus){
    if (is.null(fus_scale)) {
      stop("If 'fus' = TRUE, 'fus_scale' argument must be defined.",
           call. = FALSE)
    }
    if (is.null(fus_sample)) {
      stop("If 'fus' = TRUE, 'fus_sample' argument must be defined.",
           call. = FALSE)
    }
    if (is.null(bias)) {
      stop("If 'fus' = TRUE, 'bias' argument must be defined.",
           call. = FALSE)
    }
  }

  if (!fus){
    if (!is.null(fus_scale)) {
      stop("'fus_scale' is defined, but 'fus' = FALSE.", call. = FALSE)
    }
    if (!is.null(fus_sample)) {
      stop("'fus_sample' is defined, but 'fus' = FALSE.", call. = FALSE)
    }
    if (!is.null(bias)) {
      stop(
        "'bias' is defined, but 'fus' = FALSE.
        Response bias is only simulated in follow up survey.",
        call. = FALSE
      )
    }
  }

    if (!is.null(fus_scale)) {
      if (fus_scale > 1) {
      message(
        "fus_scale > 1; Hunters more likely to respond to follow up
        than to initial survey."
      )
    }
  }

  # Actual function ----
  if (!fus){

    N <- x$pop_size[[1]]
    bias <- rep("NA", times = length(resp))

    # Later passed into a map2() function that creates a list with a new
    # element for each response scenario.
    single_sim_m1 <- function(dat, .r){
      mutate(
        .data = dat,
        method = "mandatory",
        resp_rate = .r,
        init_resp =
          case_when(
            harvest == 1 ~ rbinom(N, 1, resp_rate),
            harvest == 0 ~ 0L
          )
      ) %>%
        select(method, pop_size, true_harvest, tidyselect::everything())
    }

    # This scenario needs its own multi_sim function because there is no
    # reporting bias:
    multi_sim_m1 <- function(){
      sim_list <- purrr::map(1:length(resp), ~x)
      sim_list <- purrr::map2(sim_list, resp, single_sim_m1)

      names(sim_list) <- paste0("resp ", resp, "; bias NA")
      return(sim_list)
    }

    out <- vector(mode = "list", length = times)
    for(i in 1:times){
      out[[i]] <- multi_sim_m1()
    }
    names(out) <- paste("Sim", 1:length(out))
    return(out)

  } else if (fus) {

    N <- x$pop_size[[1]]

    single_sim_m2 <- function(dat, .r, .b){
      mutate(
        .data = dat,
        method = "mandatory",
        resp_bias = .b,
        resp_rate = .r,
        init_resp = case_when(
          harvest == 1 ~ rbinom(N, 1, resp_rate),
          harvest == 0 ~ 0L
        ),
        fus_sample = case_when(
          init_resp == 1 ~ NA_integer_,
          init_resp == 0 ~ rbinom(N, 1, fus_sample)),
        fus_uns_resp_rate = resp_rate * fus_scale,
        fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
        fus_suc_resp_rate = resp_rate * resp_bias * fus_scale,
        fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
        fus_resp = case_when(
          init_resp == 1 ~ NA_integer_,
          fus_sample == 0 ~ NA_integer_,
          harvest == 1 ~ rbinom(N, 1, fus_suc_resp_rate),
          harvest == 0 ~ rbinom(N, 1, fus_uns_resp_rate)
        )
      ) %>%
        select(method, pop_size, true_harvest, tidyselect::everything())
    }

    out <- vector(mode = "list", length = times)
    for(i in 1:times){
      out[[i]] <- multi_sim(x, resp, bias, single_sim_m2)
    }
    names(out) <- paste("Sim", 1:length(out))
    return(out)
  }
}

# simple() =====================================================================
#' @rdname survey
#' @export

simple <- function(x, sample, resp, bias,
                   fus = FALSE, fus_scale = NULL, times = 1) {

  # Argument checks ----
  if (times %% 1 != 0){
    stop ("'times' must be a whole number.", call. = FALSE)
  }

  argcheck <- c(sample, resp)

  if (any(argcheck > 1) | any(argcheck <= 0)) {
    stop ("'sample' and 'resp' must only contain probabilities and be > 0.",
          call. = FALSE
    )
  }

  if (fus & is.null(fus_scale)) {
    stop ("If 'fus' = TRUE, 'fus_scale' argument must be defined.",
          call. = FALSE)
  }

  if (!fus & !is.null(fus_scale)) {
    stop ("'fus_scale' is defined, but 'fus' = FALSE.",
          call. = FALSE)
  }

  if (!is.null(fus_scale)) {
    if (fus_scale > 1) {
      message(
        "fus_scale > 1; Hunters more likely to respond to follow up
        than to initial survey."
      )
    }
  }

  if (any(bias < 1)) {
    message(
      "At least 1 value of 'bias' < 1; successful hunters will be less
      likely to respond to survey than unsuccessful hunters."
    )
  }

  # Actual function ----
  N <- x$pop_size[[1]]

  single_sim_s <- function(dat, .r, .b){
    out <- mutate(
      dat,
      method = "simple",
      sample = rbinom(N, 1, sample),
      resp_bias = .b,
      uns_resp_rate = .r,
      suc_resp_rate = .r * .b,
      suc_resp_rate = changeto1(suc_resp_rate),
      init_resp =
        case_when(
          sample  == 0 ~ NA_integer_,
          harvest == 1 ~ rbinom(N, 1, suc_resp_rate),
          harvest == 0 ~ rbinom(N, 1, uns_resp_rate)
        )
    )

    if (fus) {
      out <- mutate(
        out,
        fus_uns_resp_rate = uns_resp_rate * fus_scale,
        fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
        fus_suc_resp_rate = suc_resp_rate * fus_scale,
        fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
        fus_resp =
          case_when(
            sample == 0 ~ NA_integer_,
            init_resp == 1 ~ NA_integer_,
            harvest == 1 ~ rbinom(N, 1, fus_suc_resp_rate),
            harvest == 0 ~ rbinom(N, 1, fus_uns_resp_rate)
          )
      )
    }

    out <- out %>%
      select(method, pop_size, true_harvest, tidyselect::everything())

    return(out)
  }

  out <- vector(mode = "list", length = times)
  for(i in 1:times){
    out[[i]] <- multi_sim(x, resp, bias, single_sim_s)
  }

  names(out) <- paste("Sim", 1:length(out))
  return(out)
}

# vol() ========================================================================
#' @rdname survey
#' @export

vol <- function(x, resp, bias, fus = FALSE,
                fus_sample = NULL, fus_scale = NULL, times = 1) {

  #Argument checks ----
  if (times %% 1 != 0){
    stop ("'times' must be a whole number.", call. = FALSE)
  }

  argcheck <- c(fus_sample, resp)
  if (any(argcheck > 1) | any(argcheck <= 0)){
    stop (
      "'resp' and 'fus_sample' must be proportions and > 0.",
      call. = FALSE
    )
  }

  if (fus & (is.null(fus_scale) | is.null(fus_sample))){
    stop (
      "If 'fus' = TRUE, 'fus_scale' and 'fus_sample' arguments
      must be defined.",
      call. = FALSE
    )
  }

  if (!fus & (!is.null(fus_scale) | !is.null(fus_sample))){
    stop ("'fus_scale' and/or 'fus_sample' are defined, but 'fus' = FALSE.",
          call. = FALSE)
  }

  if (!is.null(fus_scale)){
    if (fus_scale > 1){
      message(
        "fus_scale > 1; Hunters more likely to respond to follow up
        than to voluntarily report"
      )
    }
  }

  if (any(bias < 1)) {
    message(
      "At least 1 value of 'bias' < 1; successful hunters will be
      less likely to respond to survey than unsuccessful hunters."
    )
  }

  # Actual function ----
  N <- x$pop_size[[1]]

  single_sim_v <- function(dat, .r, .b){
      out <- mutate(
        dat,
        method = "voluntary",
        resp_bias = .b,
        uns_resp_rate = .r,
        suc_resp_rate = .r * .b,
        suc_resp_rate = changeto1(suc_resp_rate),
        init_resp = case_when(
          harvest == 1 ~ rbinom(N, 1, suc_resp_rate),
          harvest == 0 ~ rbinom(N, 1, uns_resp_rate)
        )
      )

    if (fus){
      out <- out %>%
        mutate(
          fus_uns_resp_rate = uns_resp_rate * fus_scale,
          fus_suc_resp_rate = suc_resp_rate * fus_scale,
          fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
          fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
          fus_sample =
            case_when(
              init_resp == 0 ~ rbinom(N, 1, fus_sample),
              init_resp == 1 ~ NA_integer_
            ),
          fus_resp =
            case_when(
              init_resp == 1 ~ NA_integer_,
              fus_sample == 0 ~ NA_integer_,
              harvest == 1 ~ rbinom(N, 1, fus_suc_resp_rate),
              harvest == 0 ~ rbinom(N, 1, fus_uns_resp_rate)
            )
        )
    }

    out <- out %>%
      select(method, pop_size, true_harvest, tidyselect::everything())
    return(out)
  }

  out <- vector(mode = "list", length = times)
  for(i in 1:times){
    out[[i]] <- multi_sim(x, resp, bias, single_sim_v)
  }

  names(out) <- paste("Sim", 1:length(out))
  return(out)
}
