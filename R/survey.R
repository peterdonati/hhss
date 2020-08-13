#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate case_when
NULL

#' Survey simulations
#'
#' @name survey
#'
#' @aliases mand simple vol
#' @description
#' The survey functions take an output from \code{\link{pop}} and simulate
#' survey responses based on the method specified.
#' \itemize{
#' \item \code{simple()} creates a simulation where a population of hunters
#' are surveyed using a simple random sample.
#' \item \code{mand()} creates a simulation where only
#' successful hunters are mandated to report.
#' \item \code{vol()} creates a simulation where
#' reporting is voluntary, and both successful and unsuccessful hunters report.
#' Follow up surveys are completed by simple random sampling.
#' }
#'
#' @details If any scaling arguments scale probabilities to be > 1, the
#' probabilities will silently be changed to 1.
#' \cr\cr
#' The \code{times} argument does not necessarily tell you how many
#' simulations are completed in total. That is only true when \code{resp} and
#' \code{bias} contain one value each. For example, say you define
#' \code{resp = c(0.4, 0.6)}, \code{bias = c(1, 1.1, 1.2)}, and
#' \code{times = 100}. There will indeed be 100 simulations, but within each
#' of those simulations there will also be a new simulation for every
#' value within \code{resp} and then that will be repeated for every
#' value within \code{bias}. So in this case there would be 600 simulations:
#' \code{times} * (the length of \code{resp}) * (the length of \code{bias}).
#' \cr\cr
#' The same population remains unchanged for all response simulations. This
#' means that the variables "method", "pop_size", "true_harvest", "group",
#' and "harvest" that are reported in the output will always be the same
#' for every simulation. These columns will also repeat themselves every
#' \code{n} rows within a single "Response sim *" element of the outputted list.
#'
#' @param x An output from \code{pop}. The population to simulate response for.
#' @param sample Probability a hunter is sampled for a survey
#' @param resp Probability/probabilities of response.
#' \itemize{
#' \item In \code{simple()} and \code{vol()} it defines response
#' probabilities for unsuccessful hunters.
#' \item In \code{mand()} it defines
#' response probabilities for successful hunters, as they are the only ones
#' mandated to report.
#' }
#' @param bias The rate(s) of response for successful hunters, relative to
#' unsuccessful hunters. Introduces response bias if any value not equal to 1.
#' @param fus Logical. If \code{TRUE}, a single follow up survey will be
#' simulated.
#' @param fus_scale Scales initial response probabilities,
#' creating new probabilities of response for follow up surveys.
#' @param fus_sample Probability that a non-respondent is sampled for a follow
#' up survey.
#' @param times The number of times to simulate responses from
#' the same population.
#'
#' @return A list, where the length is equal to \code{times}. Each element in
#' the list is a tibble where each row represents a hunter.
#' A single tibble contains some of these variables:
#' \itemize{
#' \item \code{method}: The survey method that was used to gather responses.
#' \item \code{pop_size}: The population size.
#' \item \code{true_harvest}: The sum of harvests from the population.
#' \item \code{group}: The group in which the hunter was placed.
#' \item \code{harvest}: "1" for a successful hunter, and "0" if unsuccessful.
#' \item \code{sample}: "1" if the hunter was asked to participate in the
#' initial survey, "0" otherwise.
#' \item \code{resp_bias}: The response bias currently being simulated.
#' \item \code{resp_rate}:
#' \item \code{uns_resp_rate}: The probability at which a hunter will respond
#' to an initial survey if they were unsuccessful.
#' \item \code{suc_resp_rate}: The probability at which a hunter will respond
#' to an initial survey if they were successful.
#' \item \code{init_resp}: "1" if the hunter responded to the initial survey,
#' "0" otherwise.
#' \item \code{fus_uns_resp_rate}: The probability at which a hunter will
#' respond to a follow up survey if they were unsuccessful.
#' \item \code{fus_suc_resp_rate}: The probability at which a hunter will
#' respond to a follow up survey if they were successful.
#' \item \code{fus_sample}: "1" if the hunter was asked to participate in a
#' follow up survey, "0" otherwise.
#' \item \code{fus_resp}: "1" if they responded to the follow up survey,
#' "0" otherwise.
#' }
#'
#' @examples
#' # First, create a population:
#' dat <- pop(n = 1000, split = 0.7, success1 = 0.25, success0 = 0.6)
#'
#' # Simulate a simple random sample from that population:
#' simple(
#'   dat,
#'   sample = 0.4,
#'   resp = 0.3,
#'   bias = 1,
#'   times = 10
#'   )
#'
#' # Vectors can be passed to resp and bias:
#' vol(
#'   dat,
#'   resp = seq(0.3, 0.8, 0.1),
#'   bias = c(1, 1.1, 1.2),
#'   fus = TRUE,
#'   fus_scale = 1.2,
#'   fus_sample = 0.2,
#'   times = 10
#'   )
#'
# mand() =======================================================================
#' @rdname survey

mand <- function(x, resp, bias, fus = FALSE,
                 fus_sample = NULL, fus_scale = NULL, times = 1){

  argcheck <- c(resp, fus_sample)
  if (any(argcheck > 1) | any(argcheck < 0)) {
    stop("'resp' and/or 'fus_sample' must be proportions.", call. = FALSE)
  }

  if (fus & is.null(fus_scale)) {
    stop("If 'fus' = TRUE, 'fus_scale' argument must be defined.",
         call. = FALSE)
  }

  if (fus & is.null(fus_sample)) {
    stop("If 'fus' = TRUE, 'fus_sample' argument must be defined.",
         call. = FALSE)
  }

  if (!fus & !is.null(fus_scale)) {
    stop("'fus_scale' is defined, but 'fus' = FALSE.",
         call. = FALSE)
  }

  if (!fus & !is.null(fus_sample)) {
    stop("'fus_sample' is defined, but 'fus' = FALSE.",
         call. = FALSE)
  }

  if (!is.null(fus_scale)) {
    if (fus_scale > 1) {
      message(
        paste0("fus_scale > 1; Hunters more likely to respond to follow up",
               " than to initial survey.")
      )
    }
  }

  if (!fus){
    n <- x$pop_size[[1]]
    survsim <- function(fillthis){
      full_sim <- purrr::map_dfr(seq_along(resp), ~ x) %>%
        mutate(
          method    = "mandatory",
          resp_rate = rep(resp, each = n),
          init_resp =
            case_when(
              harvest == 1 ~ rbinom(n*length(resp), 1, resp_rate),
              harvest == 0 ~ 0L
            )
        )
      full_sim <- full_sim %>%
        select(method, pop_size, true_harvest, tidyselect::everything())
      return(full_sim)
    }
    out <- purrr::map(1:times, survsim)
    names(out) <- paste("Response sim", 1:length(out))
    return(out)
  }

  if(fus){

    lr <- length(resp)
    lb <- length(bias)
    N <- x$pop_size[[1]]

    changeto1 <- function(x) {
      case_when(
        x > 1 ~ 1,
        TRUE  ~ x
      )
    }

    survsim <- function(fillthis){
      full_sim <- map_dfr(1:(lr * lb), ~x)
      full_sim <- mutate(
        full_sim,
        method = "mandatory",
        resp_bias = rep(bias, each = N * lr),
        resp_rate = rep(resp, each = N, times = lb),
        init_resp = case_when(
          harvest == 1 ~ rbinom(nrow(full_sim), 1, resp_rate),
          harvest == 0 ~ 0L
        ),
        fus_sample = case_when(
          init_resp == 1 ~ NA_integer_,
          init_resp == 0 ~ rbinom(nrow(full_sim), 1, fus_sample)),
        fus_uns_resp_rate = resp_rate * fus_scale,
        fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
        fus_suc_resp_rate = resp_rate * resp_bias * fus_scale,
        fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
        fus_resp = case_when(
          init_resp == 1 ~ NA_integer_,
          fus_sample == 0 ~ NA_integer_,
          harvest == 1 ~ rbinom(nrow(full_sim), 1, fus_suc_resp_rate),
          harvest == 0 ~ rbinom(nrow(full_sim), 1, fus_uns_resp_rate)
        )
      )
    }

    out <- purrr::map(1:times, survsim)
    names(out) <- paste("Response sim", 1:length(out))
    return(out)
  }
}


# simple() =====================================================================
#' @rdname survey

simple <- function(x, sample, resp, bias,
                   fus = FALSE, fus_scale = NULL, times = 1) {
  argcheck <- c(sample, resp)
  if (any(argcheck > 1) | any(argcheck < 0)) {
    stop ("'sample' and 'resp' must only contain probabilities.",
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
        paste0("fus_scale > 1; Hunters more likely to respond to follow up",
               " than to initial survey."),
        call. = FALSE
      )
    }
  }
  if (any(bias < 1)) {
    message(
      paste0("At least 1 value of 'bias' < 1; successful hunters will be less",
             " likely to respond to survey than unsuccessful hunters.")
    )
  }
  # used when scaling arguments scale probabilities to > 1:
  changeto1 <- function(y) {
    case_when(
      y > 1 ~ 1,
      TRUE  ~ y
    )
  }

  # survsim() ==================================================================
  n <- x$pop_size[[1]]
  survsim <- function(fillthis){
    full_sim <- purrr::map_dfr(1:(length(resp) * length(bias)), ~ x)
    full_sim <- mutate(
      full_sim,
      method        = "simple",
      sample        = rbinom(nrow(full_sim) , 1, sample),
      resp_bias     = rep(bias, each = n * length(resp)),
      uns_resp_rate = rep.int(rep(resp, each = n), length(bias)),
      suc_resp_rate = rep.int(rep(resp, each = n), length(bias)) * resp_bias,
      suc_resp_rate = changeto1(suc_resp_rate),
      init_resp =
        case_when(
          sample  == 0 ~ NA_integer_,
          harvest == 1 ~ rbinom(n * (length(resp) * length(bias)),
                                1,
                                suc_resp_rate),
          harvest == 0 ~ rbinom(n * (length(resp) * length(bias)),
                                1,
                                uns_resp_rate)
        )
    )
    if (fus) {
      full_sim <- mutate(
        full_sim,
        fus_uns_resp_rate = uns_resp_rate * fus_scale,
        fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
        fus_suc_resp_rate = suc_resp_rate * fus_scale,
        fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
        fus_resp =
          case_when(
            sample    == 0  ~ NA_integer_,
            init_resp == 1  ~ NA_integer_,
            harvest   == 1  ~ rbinom(n * (length(resp) * length(bias)),
                                     1,
                                     fus_suc_resp_rate),
            harvest   == 0  ~ rbinom(n * (length(resp) * length(bias)),
                                     1,
                                     fus_uns_resp_rate)
          )
      )
    }
    full_sim <- full_sim %>%
      select(method, pop_size, true_harvest, tidyselect::everything())
    return(full_sim)
  }
  # Output =====================================================================
  out <- purrr::map(1:times, survsim)
  names(out) <- paste("Response sim", 1:length(out))
  return (out)
}

# vol() ========================================================================
#' @rdname survey

vol <- function(x, resp, bias, fus = FALSE,
                fus_sample = NULL, fus_scale = NULL, times = 1) {

  argcheck <- c(fus_sample, resp)
  if (any(argcheck > 1) | any(argcheck < 0)){
    stop (
      paste0("1 or more arguments that must contain proportions or",
             " probabilities are < 0 or > 1."),
      call. = FALSE
    )
  }
  if (fus & (is.null(fus_scale) | is.null(fus_sample))){
    stop (
      paste0("If 'fus' = TRUE, 'fus_scale' and 'fus_sample' arguments",
             " must be defined.")
      , call. = FALSE
    )
  }
  if (!fus & (!is.null(fus_scale) | !is.null(fus_sample))){
    stop ("'fus_scale' and/or 'fus_sample' are defined, but 'fus' = FALSE.",
          call. = FALSE)
  }
  if (!is.null(fus_scale)){
    if (fus_scale > 1){
      message(
        paste0("fus_scale > 1; Hunters more likely to respond to follow up",
               " than to voluntarily report")
      )
    }
  }
  if (any(bias < 1)) {
    message(
      paste0("At least 1 value of 'bias' < 1; successful hunters will be",
             " less likely to respond to survey than unsuccessful hunters.")
    )
  }
  # used when scaling arguments scale probabilities to > 1:
  changeto1 <- function(y){
    case_when(
      y > 1 ~ 1,
      TRUE  ~ y
    )
  }

  # survsim()===================================================================
  lb <- length(bias)
  lr <- length(resp)
  n  <- x$pop_size[[1]]
  survsim <- function(fillthis){
    full_sim <- purrr::map_dfr(1:(lr * lb), ~ x) %>%
      mutate(
        method        = "voluntary",
        resp_bias     = rep(bias, each = n * lr),
        uns_resp_rate = rep.int(rep(resp, each = n), lb),
        suc_resp_rate = rep.int(rep(resp, each = n), lb) * resp_bias,
        suc_resp_rate = changeto1(suc_resp_rate),
        init_resp     =
          case_when(
            harvest == 1 ~ rbinom(n * lr * lb, 1, suc_resp_rate),
            harvest == 0 ~ rbinom(n * lr * lb, 1, uns_resp_rate)
          )
      )
    if (fus){
      full_sim <- full_sim %>%
        mutate(
          fus_uns_resp_rate = uns_resp_rate * fus_scale,
          fus_suc_resp_rate = suc_resp_rate * fus_scale,
          fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
          fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
          fus_sample        =
            case_when(
              init_resp == 0 ~ rbinom(n * lr * lb, 1, fus_sample),
              init_resp == 1 ~ NA_integer_
            ),
          fus_resp          =
            case_when(
              init_resp  == 1    ~ NA_integer_,
              fus_sample == 0    ~ NA_integer_,
              harvest    == 1    ~ rbinom(n * lr * lb, 1, fus_suc_resp_rate),
              harvest    == 0    ~ rbinom(n * lr * lb, 1, fus_uns_resp_rate)
            )
        )
    }
    full_sim <- full_sim %>%
      select(method, pop_size, true_harvest, tidyselect::everything())
    return(full_sim)
  }

  # Output =====================================================================
  out <- purrr::map(1:times, survsim)
  names(out) <- paste("Response sim", 1:length(out))
  return (out)
}
