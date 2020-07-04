#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate case_when
NULL

# pop functions documentation ==================================================
#' Population and harvest survey simulations
#'
#'
#' @name pop
#'
#' @aliases pop_simple pop_mand pop_vol
#' @description
#' The pop functions will simulate a population of hunters and whether or not
#' they were successful in harvesting. If a survey method is specified,
#' responses are also simulated. \cr
#' \itemize{
#' \item \code{pop()} creates a population of hunters and simulates
#' harvest.
#' \item \code{pop_simple()} creates a simulation where a population of hunters
#' are surveyed using a simple random sample.
#' \item \code{pop_mand()} creates a simulation where only
#' successful hunters are mandated to report.
#' \item \code{pop_vol()} creates a simulation where
#' reporting is voluntary, and both successful and unsuccessful hunters report.
#' Follow up surveys are completed by simple random sampling.
#' }
#'
#' @details If any scaling arguments scale probabilities to be > 1, the
#' probabilities will silently be changed to 1. \cr\cr
#' The \code{times} argument does not necessarily tell you how many
#' simulations are completed in total. That is only true when \code{resp} and
#' \code{bias} only contain one value each. For example, say you define
#' \code{resp = c(0.4, 0.6)}, \code{bias = c(1, 1.1, 1.2)}, and
#' \code{times = 100}. There will indeed be 100 simulations, but within each
#' of those simulations there will also be a new simulation for every
#' value within \code{resp} and then that will be repeated for every
#' value within \code{bias}. So in this case there will be 600 simulations:
#' \code{times} * the length of \code{resp} * the length of \code{bias}.\cr\cr
#' The same population is used for all response simulations. This
#' means that the variables "method", "pop_size", "true_harvest", "group",
#' and "harvest" that are reported in the output will always be the same
#' for every simulation. These columns will also repeat themselves every
#' \code{n} rows within a single "Response sim x" element of the outputted list.
#'
#' @param n The desired population size.
#' @param split Proportion of the population that is placed into group 1.
#' The remaining proportion (i.e. \code{1 - split}) will be in group 0.
#' @param success1 Probability of a hunter in group 1 to harvest
#' @param success0 Probability of a hunter in group 0 to harvest
#' @param sample Probability a hunter is sampled for a survey
#' @param resp Probability/probabilities of response. Multiple values can be
#' passed to it.
#' \itemize{
#' \item In \code{pop_simple()} and \code{pop_vol()} it defines response
#' probabilities for unsuccessful hunters.
#' \item In \code{pop_mand()} it defines
#' response probabilities for successful hunters, as they are the only ones
#' mandated to report.
#' }
#' @param bias Introduces response bias if not equal to one. Scales \code{resp}
#' to create probabilities of response for successful hunters. Multiple values
#' can be passed to it.
#' @param fus Logical. If TRUE, a single follow up survey will be simulated.
#' @param fus_scale Scales initial response probabilities,
#' creating new probabilities of response for follow up surveys.
#' @param fus_sample Probability that a non-respondent is sampled for a follow
#' up survey.
#' @param times The number of times to simulate responses from
#' the same population.
#'
#' @return A list, where the length is equal to \code{times}. Each element in
#' the list is a single tibble where each row represents a hunter.
#' The tibble will contain some of these variables:
#' \itemize{
#' \item \strong{method}: The survey method that was used to gather responses.
#' \item \strong{pop_size}: The population size.
#' \item \strong{true_harvest}: The sum of harvests from the population.
#' \item \strong{group}: The group in which the hunter was placed.
#' \item \strong{harvest}: "1" for a successful hunter, and "0" if unsuccessful.
#' \item \strong{sample}: "1" if the hunter was asked to participate in the
#' initial survey, "0" otherwise.
#' \item \strong{resp_bias}: The response bias currently being simulated.
#' \item \strong{uns_resp_rate}: The probability at which a hunter will respond
#' to an initial survey if they were unsuccessful.
#' \item \strong{suc_resp_rate}: The probability at which a hunter will respond
#' to an initial survey if they were successful.
#' \item \strong{init_resp}: "1" if the hunter responded to the initial survey,
#' "0" otherwise.
#' \item \strong{fus_uns_resp_rate}: The probability at which a hunter will
#' respond to a follow up survey if they were unsuccessful.
#' \item \strong{fus_suc_resp_rate}: The probability at which a hunter will
#' respond to a follow up survey if they were successful.
#' \item \strong{fus_sample}: "1" if the hunter was asked to participate in a
#' follow up survey, "0" otherwise.
#' \item \strong{fus_resp}: "1" if they responded to the follow up survey,
#' "0" otherwise.
#' }
#'
#' @examples
#' # Simulate a dataset that contains a population of 1,000 hunters and
#' # simulates harvest where each hunter has a probability of 0.3 to harvest:
#' pop(n = 1000, success1 = 0.3)
#'
#' # A similar population as above example, but with a simulation where
#' # successful hunters are mandated to report. In this case, only ~70% of them
#' # actually report:
#' pop_mand(
#'   n          = 1000,
#'   success1   = 0.3,
#'   resp       = 0.7,
#' )
#'
#' # A population where ~70% of hunters are in group 1 and
#' # harvest with a probability of 0.25, while the remaining ~30%
#' # are in group 0 and harvest with a probability of 0.4. Hunters are
#' # sampled by simple random sampling  at a probability of 0.5. Simulate
#' # multiple levels of response, where successful hunters are 1.2 times
#' # more likely to respond than unsuccessful hunters:
#' pop_simple(
#'   n          = 1000,
#'   split      = 0.7,
#'   success1   = 0.25,
#'   success0   = 0.4,
#'   sample     = 0.5,
#'   resp       = seq(0.3, 0.8, 0.1),
#'   resp_bias  = 1.2
#' )
#'
#' # Simulate a similar population as the one in the example above.
#' # Except this time complete a follow up survey of the
#' # non-respondents, where hunters are only 0.7 times as likely
#' # (i.e. ~30% less likely) to respond to the follow up survey as they
#' # were to the initial survey:
#' pop_simple(
#'   n          = 1000,
#'   split      = 0.7,
#'   success1   = 0.4,
#'   success0   = 0.25,
#'   sample     = 0.5,
#'   resp       = seq(0.3, 0.8, 0.1),
#'   resp_bias  = 1.2,
#'   fus        = TRUE,
#'   fus_scale  = 0.7
#' )
#'
#' # Again, a similar population as the one in the example above.
#' # But now simulate more levels of response bias and repeat the simulation
#' # 100 times.
#' pop_simple(
#'   n          = 1000,
#'   split      = 0.7,
#'   success1   = 0.4,
#'   success0   = 0.25,
#'   sample     = 0.5,
#'   resp       = seq(0.3, 0.8, 0.1),
#'   resp_bias  = c(1, 1.1, 1.2),
#'   fus        = TRUE,
#'   fus_scale  = 0.7,
#'   times      = 100
#' )
#'
#' # A voluntary scenario:
#' pop_vol(
#'   n          = 1000,
#'   split      = 0.7,
#'   success1   = 0.4,
#'   success0   = 0.25,
#'   resp       = c(0.3, 0.6, 1),
#'   resp_bias  = c(1, 1.1, 1.2, 1.3),
#'   fus        = TRUE,
#'   fus_scale  = 1.2,
#'   fus_sample = 0.2,
#'   times      = 100
#' )

# pop() ========================================================================

pop <- function(n, split = 1, success1, success0 = success1){
  # Argument checks ============================================================

  argcheck <- c(split, success1, success0)
  if (any(argcheck > 1) | any(argcheck < 0)){
    stop (
      paste0("1 or more arguments that must contain proportions or",
             " probabilities are < 0 or > 1. See ?pop"),
      call. = FALSE
    )
  }

  if (split != 1 & success0 == success1){
    warning (
      paste0("Population split into two groups, but both groups have same",
             " probability of harvest. Consider specifying argument",
             " 'success0'."),
      call. = FALSE
    )
  }

  if ((split == 1 | split == 0) & success0 != success1){
    warning(
      paste0("Population is not 'split' into different groups, and 'success1'",
             " does not equal 'success0'. To simulate different harvest",
             " rates, the population must be 'split' into two groups."),
      call. = FALSE
    )
  }

  # Create population ==========================================================
  pop <- tibble::tibble(
    pop_size     = n,
    group        = rbinom(n, 1, split),
    harvest      =  case_when(
      group == 1 ~ rbinom(n, 1, success1),
      group == 0 ~ rbinom(n, 1, success0)),
    true_harvest = sum(harvest),
  )

  # Output =====================================================================
  # Rearrange tibble so pop_size and true_harvest always 1st and 2nd vars,
  # to be consistent with other pop functions.
  pop <- select(pop, pop_size, true_harvest, tidyselect::everything())

  return(pop)
}

# pop_mand() ==============================================================
#' @rdname pop

pop_mand <- function(n, split = 1, success1, success0 = success1,
                     resp, times = 1){

  # Argument checks ============================================================

  argcheck <- c(split, success1, success0, resp)
  if (any(argcheck > 1) | any(argcheck < 0)){
    stop (
      paste0("1 or more arguments that must contain proportions or",
             " probabilities are < 0 or > 1. See ?pop"),
      call. = FALSE
    )
  }

  if (split != 1 & success0 == success1){
    warning (
      paste0("Did you mean to define argument 'success0'? Population is",
             " 'split' into two groups, but both groups have same probability",
             " of harvest"),
      call. = FALSE
    )
  }

  if ((split == 1 | split == 0) & success0 != success1){
    warning (
      paste0("Did you mean to define argument 'split'? Different harvest",
             " rates will not be simulated unless population is 'split'."),
      call. = FALSE
    )
  }

  # Create initial population data =============================================
  init_pop <- tibble::tibble(
    method       = "mandatory",
    pop_size     = n,
    group        = rbinom(n, 1, split),
    harvest      =  case_when(
      group == 1 ~ rbinom(n, 1, success1),
      group == 0 ~ rbinom(n, 1, success0)
      ),
    true_harvest = sum(harvest)
  )

  # Simulate response ==========================================================

  # Survey simulation function, to later be passed to map() .f argument:
  # The 'fillthis' argument holds an empty element from the final output list.
  survsim <- function(fillthis){
    full_sim <- purrr::map_dfr(seq_along(resp), ~ init_pop) %>%
      mutate(
        resp_rate = rep(resp, each = n),
        init_resp =
          case_when(
            harvest == 1 ~ rbinom(n*length(resp), 1, resp_rate),
            harvest == 0 ~ 0L
          )
      )

    # Rearrange tibble to be consistent with other pop functions
    full_sim <- full_sim %>%
      select(method, pop_size, true_harvest, tidyselect::everything())

    return(full_sim)
  }
  # Output =====================================================================

  out <- purrr::map(1:times, survsim)
  names(out) <- paste("Response sim", 1:length(out))
  return(out)
}

# pop_simple() =================================================================
#' @rdname pop

pop_simple <- function(n, split = 1, success1, success0 = success1, sample,
                       resp, bias, fus = FALSE,
                       fus_scale = NULL, times = 1) {
  # Argument checks ============================================================

  argcheck <- c(split, success1, success0, sample, resp)
  if (any(argcheck > 1) | any(argcheck < 0)) {
    stop (
      paste0("1 or more arguments that must contain proportions or",
             " probabilities are < 0 or > 1. See ?pop"),
      call. = FALSE
    )
  }

  if (split != 1 & success0 == success1) {
    warning (
      paste0("Did you mean to define argument 'success0'? Population is",
             " 'split' into two groups, but both groups have same probability",
             " of harvest."),
      call. = FALSE
    )
  }

  if ((split == 1 | split == 0) & success0 != success1) {
    warning(
      paste0("Did you mean to define argument 'split'? Different harvest",
             " rates will not be simulated unless population is 'split'."),
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
      warning(
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

  # changeto1 function =========================================================

  # used when scaling arguments scale probabilities to > 1.
  changeto1 <- function(x) {
    case_when(
      x > 1 ~ 1,
      TRUE  ~ x
    )
  }

  # Create initial population data =============================================

  init_pop <- tibble::tibble(
    method       = "simple",
    pop_size     = n,
    group        = rbinom(n, 1, split),
    harvest      =  case_when(
      group == 1 ~ rbinom(n, 1, success1),
      group == 0 ~ rbinom(n, 1, success0)),
    true_harvest = sum(harvest),
    sample       = rbinom(n, 1, sample)
  )

  # Simulate survey responses ==================================================

  # This function will later be passed to map() .f argument.
  # The 'fillthis' argument holds an empty element from the final output list.
  survsim <- function(fillthis){
    full_sim <- purrr::map_dfr(
      1:(length(resp) * length(bias)),
      ~ init_pop
    ) %>%
      mutate(
        resp_bias     = rep(bias, each = n * length(resp)),
        uns_resp_rate = rep.int(rep(resp, each = n), length(bias)),
        suc_resp_rate = rep.int(rep(resp, each = n), length(bias)) * resp_bias,
        # if suc_resp_rate is > 1, make it = 1, otherwise the probability
        # will not work within rbinom():
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

    # Follow up survey =========================================================

    if (fus) {
      full_sim <- full_sim %>%
        mutate(
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

    # Rearrange tibble so that output is consistent between all pop functions
    full_sim <- full_sim %>%
      select(method, pop_size, true_harvest, tidyselect::everything())

    return(full_sim)
  }

  # Output =====================================================================

  out <- purrr::map(1:times, survsim)
  names(out) <- paste("Response sim", 1:length(out))
  return (out)
}


# pop_vol() ====================================================================
#' @rdname pop

pop_vol <- function(n, split = 1, success1, success0 = success1,
                    resp, bias, fus = FALSE,
                    fus_scale = NULL, fus_sample = NULL, times = 1){

  # Argument checks ============================================================

  argcheck <- c(split, success1, success0, fus_sample, resp)
  if (any(argcheck > 1) | any(argcheck < 0)){
    stop (
      paste0("1 or more arguments that must contain proportions or",
             " probabilities are < 0 or > 1. See ?pop"),
      call. = FALSE
    )
  }

  if (split != 1 & success0 == success1){
    warning (
      paste0("Did you mean to define argument 'success0'? Population is",
             " 'split' into two groups, but both groups have same probability",
             " of harvest."),
      call. = FALSE
    )
  }

  if ((split == 1 | split == 0) & success0 != success1){
    warning(
      paste0("Did you mean to define argument 'split'? Different harvest",
             " rates will not be simulated unless population is 'split'."),
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
    if (fus_scale < 1){
      message(
        paste0("fus_scale < 1; Hunters less likely to respond to follow up",
               " than to voluntarily report")
      )
    }
  }

  if (any(bias < 1)) {
    message(
      paste0("At least 1 value of 'bias' < 1; successful hunters will be less",
             " likely to respond to survey than unsuccessful hunters.")
    )
  }

  # Function changeto1 =========================================================

  # used when scaling arguments scale probabilities to > 1
  changeto1 <- function(x){
    case_when(
      x > 1 ~ 1,
      TRUE  ~ x
    )
  }

  # Create initial population data =============================================
  init_pop <- tibble::tibble(
    method       = "voluntary",
    pop_size     = n,
    group        = rbinom(n, 1, split),
    harvest      =  case_when(
      group == 1 ~ rbinom(n, 1, success1),
      group == 0 ~ rbinom(n, 1, success0)),
    true_harvest = sum(harvest),
  )

  # Simulating response ========================================================

  # This function will later be passed to map() .f argument.
  # The 'fillthis' argument holds an empty element from the final output list.
  survsim <- function(fillthis){
    full_sim <- purrr::map_dfr(
      1:(length(resp) * length(bias)),
      ~ init_pop
    ) %>%
      mutate(
        resp_bias     = rep(bias, each = n * length(resp)),
        uns_resp_rate = rep.int(rep(resp, each = n), length(bias)),
        suc_resp_rate = rep.int(rep(resp, each = n), length(bias)) * resp_bias,
        # if suc_resp_rate is > 1, make it = 1, otherwise the probability
        # will not work within rbinom():
        suc_resp_rate = changeto1(suc_resp_rate),
        init_resp =
          case_when(
            harvest == 1 ~ rbinom(n * (length(resp) * length(bias)),
                                  1,
                                  suc_resp_rate),
            harvest == 0 ~ rbinom(n * (length(resp) * length(bias)),
                                  1,
                                  uns_resp_rate)
          )
      )

    # Follow up survey ===========================================================
    if (fus){
      full_sim <- full_sim %>%
        mutate(
          fus_uns_resp_rate = uns_resp_rate * fus_scale,
          fus_suc_resp_rate = suc_resp_rate * fus_scale,
          fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
          fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
          fus_sample = case_when(
            init_resp == 0 ~ rbinom(n * length(resp) * length(bias),
                                    1,
                                    fus_sample),
            init_resp == 1 ~ NA_integer_
          ),
          fus_resp =
            case_when(
              init_resp  == 1    ~ NA_integer_,
              fus_sample == 0    ~ NA_integer_ ,
              harvest    == 1    ~ rbinom(n * length(resp) * length(bias),
                                          1,
                                          fus_suc_resp_rate),
              harvest    == 0    ~ rbinom(n * length(resp) * length(bias),
                                          1,
                                          fus_uns_resp_rate)
            )
        )
    }

    # Rearrange tibble so that output is consistent between all pop functions
    full_sim <- full_sim %>%
      select(method, pop_size, true_harvest, tidyselect::everything())

    return(full_sim)
  }

  # Output =====================================================================

  out <- purrr::map(1:times, survsim)
  names(out) <- paste("Response sim", 1:length(out))
  return (out)
}

