#' @importFrom magrittr %>%
NULL

# estimator functions documentation ============================================
#'
#' Estimate total harvest
#'
#'
#' @description Estimates total harvest from a data set created by one of
#' the explicit \code{\link{pop}} functions.
#'
#' @param simdat Simulation data. An output from either \code{pop_simple()},
#' \code{pop_mand()}, or \code{pop_vol()}.
#' @param poststrat Logical. If \code{TRUE}, post-stratify respondents by the
#' \code{group} variable. If a follow up survey is simulated, it will also be
#' post-stratified. If \code{simdat} is from a \code{pop_mand()} output, the
#' population cannot be post-stratified.
#'
#' @details
#' This function relies on the structure of \code{simdat} being unchanged from
#' its original structure to know how to create estimates.
#' If you must manipulate the data set, create estimates
#' from it first, then manipulate it.\cr\cr
#' If estimates are to be made from a data set that contains multiple response
#' simulations, multiple response biases, and/or multiple response rates,
#' this function breaks the data set down into single populations and creates
#' an estimate for every single population. It then returns the averages for
#' these estimates.\cr\cr
#' All estimations, except those made from \code{pop_mand()} outputs,
#' use \code{survey::svydesign()} and
#' \code{survey::svytotal()} to estimate total harvest. Estimates are
#' calculated a bit different when a follow up survey is simulated in the
#' data to be estimated. In that case, two separate estimates are made using
#' \code{survey::svytotal()}. One estimate from the respondents to
#' the initial survey and the other from respondents
#' to follow up surveys. In the case of estimates from \code{pop_simple()},
#' the proportion of (hunters initially responding / hunters sampled) is
#' used to scale the initial estimate (i.e. the prop. of respondents is
#' assumed to make up the same
#' prop. of the entire population.) Then, the follow up respondents are assumed
#' to be representative of the remaining proportion of the entire population.
#' This proportion is used to scale the follow up estimate.
#' Both scaled estimates are then added together to create the combined
#' estimate. This is similar to estimates made from \code{pop_vol()},
#' except in that case, the proportion of initial respondents \emph{is} the
#' proportion of the population that responded.\cr\cr
#' \code{est_mand()} assumes 100\% reporting by successful hunters,
#' so it simply sums up the total reported harvest.\cr\cr
#' If \code{poststrat = TRUE}, \code{survey::postStratify()} is used,
#' with the \code{partial} argument set to \code{TRUE}.
#'
#' @return A tibble, containing an estimate of harvest for each level of
#' response rate and response bias. Also reports standard error, and mean
#' absolute percent error, among other metadata.
#'
#' @seealso
#' \code{\link{pop}} for how to create the data to be input to this
#' function.\cr\cr
#' \code{\link{pae}} if you do not need the raw population data and want to
#' skip this intermediate step and get straight to estimates. \cr\cr
#' \code{\link[survey]{svydesign}}, \code{\link[survey]{svytotal}}, and
#' \code{\link[survey]{postStratify}} for
#' details on how most estimates and standard errors are calculated.
#'
#' @examples
#' # Estimate harvest from a simulation where hunters report voluntarily:
#' # first, you must create the data:
#' dat <- pop_vol(
#'   n          = 1000,
#'   split      = 0.8,
#'   success1   = 0.3,
#'   success0   = 0.7,
#'   resp       = c(0.2, 0.6, 0.8),
#'   bias       = 1.2,
#'   times      = 50
#' )
#'
#' # then you can create estimates:
#' dat_ests <- est(dat)
#'
#' # if you want to post-stratify by the "group" variable:
#' dat_ests_ps <- est(dat, poststrat = TRUE)
NULL


# est() ========================================================================

est <- function(simdat, poststrat = FALSE){

  methods <- purrr::map(simdat, purrr::pluck, "method", 1)

  if (typeof(poststrat) != "logical") {
    stop ("'poststrat' must be logical")
  }

  # Mandatory estimates ========================================================
  if (all(methods == "mandatory")){

    if (poststrat){
      poststrat <- NULL
      message(
        paste0("Cannot post-stratify a pop_mand() output, 'poststrat = TRUE'",
               " ignored.")
      )
    }
    est_mand <- function(sim_elmt){
      ests <- sim_elmt %>%
        dplyr::group_by(resp_rate) %>%
        dplyr::summarise(
          est_harvest   = sum(init_resp, na.rm = TRUE),
          est_SE        = 0L,
          percent_error = (abs(est_harvest - true_harvest)
                           / true_harvest) * 100,
          .groups = "keep"
        )
      return(ests)
    }

    out <- purrr::map_dfr(simdat, est_mand) %>%
      dplyr::summarise(
        method        = methods[[1]],
        pop_size      = purrr::pluck(simdat, 1, "pop_size", 1),
        resp_bias     = NA_character_,
        true_hvst     = purrr::pluck(simdat, 1, "true_harvest", 1),
        mean_hvst_est = mean(est_harvest),
        mean_SE       = mean(est_SE),
        MAPE          = mean(percent_error),
        .groups = "drop"
      )

    out <- out %>%
      dplyr::mutate(resp_rate = as.character(resp_rate)) %>%
      dplyr::select(method:resp_bias, resp_rate, tidyselect::everything())
    return(out)

  } else if (all(methods == "simple")) {
    # SRS estimates ============================================================

    est_simp <- function(level, pop_dat){
      #These values will be used throughout function.
      thvst <- pop_dat$true_harvest[[1]]
      N <- pop_dat$pop_size[[1]]

      # Data must be filtered down to the population in question.
      pop <- pop_dat %>%
        dplyr::filter(dplyr::near(uns_resp_rate, level))

      # and filtered further to only respondents for harvest estimates.
      init_resp_only <- pop %>%
        dplyr::filter(init_resp == 1)

      init_design <- survey::svydesign(ids   = ~1,
                                       probs = nrow(init_resp_only) / N,
                                       data  = init_resp_only,
                                       fpc   = ~pop_size)

      if (poststrat){
        strata <- data.frame(
          group = c(1, 0),
          Freq  = c(sum(pop$group), N - sum(pop$group))
        )

        init_design <- survey::postStratify(init_design,
                                            ~group,
                                            strata,
                                            partial = TRUE)
      }

      init_est <- survey::svytotal(~harvest, init_design)

      # Must check if follow up survey was done, to create combined estimate
      if ("fus_resp" %in% names(pop)){
        # If level is 1, then there was nobody to follow up with, so
        # ignore this step.
        if (level < 1){
          fus_resp_only <- pop %>%
            dplyr::filter(fus_resp == 1)

          fus_design <- survey::svydesign(ids   = ~1,
                                          probs = nrow(fus_resp_only) / N,
                                          data  = fus_resp_only,
                                          fpc   = ~pop_size)
          if (poststrat){
            fus_design <- survey::postStratify(fus_design,
                                               ~group,
                                               strata,
                                               partial = TRUE)
          }

          fus_est <- survey::svytotal(~harvest, fus_design)

          # Define scaling proportions:
          init_prop <- sum(pop$init_resp, na.rm = TRUE) / sum(pop$sample)
          # assume fus respondents make the rest of the pop:
          fus_prop  <- 1 - init_prop
          combined_est <- (init_est * init_prop) + (fus_est * fus_prop)

          # following delta method, but partial derivatives will
          # always be the same:
          combined_SE <- (init_prop * survey::SE(init_est)) +
            (fus_prop * survey::SE(fus_est))

          estout <- tibble::tibble(
            resp_rate     = as.character(level),
            resp_bias     = as.character(pop$resp_bias[[1]]),
            true_harvest  = thvst,
            est_harvest   = as.vector(combined_est),
            est_SE        = as.vector(combined_SE),
            percent_error = (abs(est_harvest - thvst) / thvst) * 100
          )
        } else {
          estout <- tibble::tibble(
            resp_rate     = as.character(level),
            resp_bias     = as.character(pop$resp_bias[[1]]),
            true_harvest  = thvst,
            est_harvest   = as.vector(init_est),
            est_SE        = as.vector(survey::SE(init_est)),
            percent_error = (abs(est_harvest - thvst) / thvst) * 100
          )
        }
      } else {
        # If there was no follow up survey simulated:
        estout <- tibble::tibble(
          resp_rate     = as.character(level),
          resp_bias     = as.character(pop$resp_bias[[1]]),
          true_harvest  = thvst,
          est_harvest   = as.vector(init_est),
          est_SE        = as.vector(survey::SE(init_est)),
          percent_error = (abs(est_harvest - thvst) / thvst) * 100
        )
      }
      return(estout)
    }

    # Each element in 'simdat' must be split down to a single
    # level of response bias. This function does that, when it is the .f
    # argument in map()
    extractor <- function(sim_elmt){
      unq_bias <- unique(sim_elmt$resp_bias)
      pops <- vector(mode = "list", length = length(unq_bias))
      for (i in seq_along(unq_bias)){
        pops[[i]] <- dplyr::filter(sim_elmt, resp_bias == unq_bias[[i]])
      }
      return(pops)
    }

    splits <- purrr::map(simdat, extractor) %>%
      purrr::flatten()

    ests <- vector(mode = "list", length = length(splits))
    for (i in seq_along(splits)) {
      # This next line that contains unique() allows estimator() to further
      # filter data down to a single resp_rate, and therefore a single
      # population to create estimates from.
      ests[[i]] <- unique(splits[[i]]$uns_resp_rate) %>%
        purrr::map_dfr(est_simp, splits[[i]])
    }
    out <- ests %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(resp_bias, resp_rate) %>%
      dplyr::summarise(
        method        = "simple",
        pop_size      = simdat[[1]]$pop_size[[1]],
        true_harvest  = true_harvest[[1]],
        mean_hvst_est = mean(est_harvest),
        mean_SE       = mean(est_SE),
        MAPE          = mean(percent_error),
        .groups = "drop"
      )

    out <- out %>%
      dplyr::select(method, pop_size, resp_bias, resp_rate,
                    tidyselect::everything())
    return(out)

  } else if (all(methods == "voluntary")){
    # Voluntary estimates ======================================================
    est_vol <- function(level, pop_dat){

      # These values will be used throughout function:
      thvst <- pop_dat$true_harvest[[1]]
      N <- pop_dat$pop_size[[1]]

      # Data must be filtered down to the population in question:
      pop <- pop_dat %>%
        dplyr::filter(dplyr::near(uns_resp_rate, level))

      # and filtered further to only respondents for harvest estimates.
      init_resp_only <- pop %>%
        dplyr::filter(init_resp == 1)

      init_design <- survey::svydesign(ids   = ~1,
                                       probs = nrow(init_resp_only) / N,
                                       data  = init_resp_only,
                                       fpc   = ~pop_size)

      if (poststrat){
        strata <- data.frame(
          group = c(1, 0),
          Freq  = c(sum(pop$group), N - sum(pop$group))
        )

        init_design <- survey::postStratify(init_design,
                                            ~group,
                                            strata,
                                            partial = TRUE)
      }

      #estimate total harvest:
      init_est <- survey::svytotal(~harvest, init_design)

      # Now do the same for follow up respondents to be able to
      # create combined estimates:
      if ("fus_resp" %in% names(pop)){
        # if response level was already 1, there is nobody to follow up with,
        # so skip this step:
        if (level < 1){
          fus_resp_only <- pop %>%
            dplyr::filter(fus_resp == 1)

          fus_design <- survey::svydesign(ids   = ~1,
                                          probs = nrow(fus_resp_only) / N,
                                          data  = fus_resp_only,
                                          fpc   = ~pop_size)

          if (poststrat){
            fus_design <- survey::postStratify(fus_design,
                                               ~group,
                                               strata,
                                               partial = TRUE)
          }

          fus_est <- survey::svytotal(~harvest, fus_design)

          # The prop. of respondents to self-report IS the proportion
          # of the population who reported:
          init_prop <- mean(pop$init_resp, na.rm = TRUE)
          # assume fus respondents reflect the rest of the pop.
          fus_prop  <- 1 - init_prop
          combined_est <- (init_est * init_prop) + (fus_est * fus_prop)

          # following delta method, but partial derivatives will
          # always be the same:
          combined_SE <- (init_prop * survey::SE(init_est)) +
            (fus_prop  * survey::SE(fus_est))

          estout <- tibble::tibble(
            resp_rate     = as.character(level),
            resp_bias     = as.character(pop$resp_bias[[1]]),
            true_harvest  = thvst,
            est_harvest   = as.vector(combined_est),
            est_SE        = as.vector(combined_SE),
            percent_error = (abs(est_harvest - thvst) / thvst) * 100
          )
        } else {
          estout <- tibble::tibble(
            resp_rate     = as.character(level),
            resp_bias     = as.character(pop$resp_bias[[1]]),
            true_harvest  = thvst,
            est_harvest   = as.vector(init_est),
            est_SE        = as.vector(survey::SE(init_est)),
            percent_error = (abs(est_harvest - thvst) / thvst) * 100
          )
        }
      } else {
        # If there was no follow up survey simulated:
        estout <- tibble::tibble(
          resp_rate     = as.character(level),
          resp_bias     = as.character(pop$resp_bias[[1]]),
          true_harvest  = thvst,
          est_harvest   = as.vector(init_est),
          est_SE        = as.vector(survey::SE(init_est)),
          percent_error = (abs(est_harvest - thvst) / thvst) * 100
        )
      }
      return(estout)
    }

    # Each element in 'simdat' must be split down to a single
    # level of response bias. This function does that, when it is the .f
    # argument in map()
    extractor <- function(sim_elmt){
      unq_bias <- unique(sim_elmt$resp_bias)
      pops <- vector(mode = "list", length = length(unq_bias))
      for (i in seq_along(unq_bias)){
        pops[[i]] <- dplyr::filter(sim_elmt, resp_bias == unq_bias[[i]])
      }
      return(pops)
    }

    splits <- purrr::map(simdat, extractor) %>%
      purrr::flatten()

    ests <- vector(mode = "list", length = length(splits))
    for (i in seq_along(splits)) {
      # This next line that contains unique() allows estimator() to further
      # filter data down to a single resp_rate, and therefore a single
      # population to create estimates from.
      ests[[i]] <- unique(splits[[i]]$uns_resp_rate) %>%
        purrr::map_dfr(est_vol, splits[[i]])
    }

    out <- ests %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(resp_bias, resp_rate) %>%
      dplyr::summarise(
        method        = "voluntary",
        pop_size      = simdat[[1]]$pop_size[[1]],
        true_harvest  = true_harvest[[1]],
        mean_hvst_est = mean(est_harvest),
        mean_SE       = mean(est_SE),
        MAPE          = mean(percent_error),
        .groups = "drop"
      )

    out <- out %>%
      dplyr::select(method, pop_size, resp_bias, resp_rate,
                    tidyselect::everything())
    return(out)
  } else {
    stop (paste0("'simdat' must be an unmanipulated output from either",
                 " pop_mand(), pop_simple(), or pop_vol()."))
  }
}
