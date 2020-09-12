#' @importFrom magrittr %>%
NULL

# estimator function documentation ============================================
#'
#' Estimate total harvest
#'
#'
#' @description Estimates total harvest from a data set created by one of
#' the \code{\link{survey}} functions.
#'
#' @param simdat Simulation data. An output from either \code{pop_simple()},
#' \code{pop_mand()}, or \code{pop_vol()}.
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
#' data. In that case, two separate estimates are made using
#' \code{survey::svytotal()}. One estimate from the respondents to
#' the initial survey and the other from respondents
#' to follow up surveys. In the case of estimates from \code{pop_simple()},
#' the proportion of (hunters initially responding / hunters sampled) is
#' used to scale the initial estimate (i.e. the prop. of respondents is
#' assumed to make up the same
#' prop. of the entire population). Then, the follow up respondents are assumed
#' to be representative of the remaining proportion of the entire population.
#' This proportion is used to scale the follow up estimate.
#' Both scaled estimates are then added together to create the combined
#' estimate. This is similar to estimates made from \code{pop_vol()},
#' except in that case, the proportion of initial respondents \emph{is} the
#' proportion of the population that responded.\cr\cr
#' \code{est_mand()} assumes 100\% reporting by successful hunters,
#' so it simply sums up the total reported harvest.\cr\cr
#'
#' @return A tibble, containing an estimate of harvest for each level of
#' response rate and response bias. Also reports standard error, and mean
#' absolute percent error, among other metadata.
#'
#' @seealso
#' \code{\link{pop}} for how to create the data to be input to this
#' function.\cr\cr
#' \code{\link[survey]{svydesign}}, \code{\link[survey]{svytotal}} for
#' details on how estimates are calculated.
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
#' # Then you can create estimates:
#' dat_ests <- est(dat)
#'
#'
#' # Usage is the same for all possible inputs of "simdat".
#'


# est() ========================================================================

est <- function(simdat){

  methods <- purrr::map(simdat, purrr::pluck, "method", 1)

  # Each element in 'simdat' must be split down to a single
  # level of response bias. This function does that, when it is the .f
  # argument in map(). This function is called later on.
  extractor <- function(sim_elmt){
    unq_bias <- unique(sim_elmt$resp_bias)
    pops <- vector(mode = "list", length = length(unq_bias))
    for (i in seq_along(unq_bias)){
      pops[[i]] <- dplyr::filter(sim_elmt, resp_bias == unq_bias[[i]])
    }
    return(pops)
  }

  if (all(methods == "mandatory")){
    # Mandatory estimates ======================================================

    if ("fus_resp" %in% names(simdat[[1]])){
      est_mand <- function(level, pop_dat){
        thvst <- pop_dat$true_harvest[[1]]
        N <- pop_dat$pop_size[[1]]

        pop <- pop_dat %>%
          dplyr::filter(dplyr::near(resp_rate, level))

        init_est <- sum(pop$init_resp)

        if ("fus_resp" %in% names(pop)){
          fus_resp_only <- pop %>%
            dplyr::filter(fus_resp == 1)

          fus_resp_only <- dplyr::mutate(
            fus_resp_only,
            fpc = N - sum(pop$init_resp)
          )

          fus_design <- survey::svydesign(ids   = ~1,
                                          probs = nrow(fus_resp_only) /
                                            (N - sum(pop$init_resp)),
                                          data  = fus_resp_only,
                                          fpc   = ~fpc)

          fus_est <- survey::svytotal(~harvest, fus_design)
        }

        combined_est <- init_est + fus_est

        estout <- tibble::tibble(
          resp_rate    = as.character(level),
          resp_bias    = as.character(pop$resp_bias[[1]]),
          true_harvest = thvst,
          est_harvest  = as.vector(combined_est),
          est_SE       = as.vector(survey::SE(fus_est)),
          ARE          = abs((est_harvest - true_harvest) / true_harvest),
          sqer         = ((est_harvest - true_harvest)^2),
        )
        return(estout)
      }

      # split up single levels of response bias:
      splits <- purrr::map(simdat, extractor) %>%
        purrr::flatten()

      ests <- vector(mode = "list", length = length(splits))
      for (i in seq_along(splits)) {
        # This next line that contains unique() allows estimator() to further
        # filter data down to a single resp_rate, and therefore a single
        # population to create estimates from.
        ests[[i]] <- unique(splits[[i]]$resp_rate) %>%
          purrr::map_dfr(est_mand, splits[[i]])
      }

      out <- ests %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(resp_bias, resp_rate) %>%
        dplyr::summarise(
          method        = "mandatory",
          pop_size      = simdat[[1]]$pop_size[[1]],
          true_hvst     = true_harvest[[1]],
          mean_hvst_est = mean(est_harvest),
          mean_SE       = mean(est_SE),
          MARE          = mean(ARE),
          RRMSE         = sqrt(mean(sqer)) / mean(true_hvst),
          .groups = "drop"
        )

      out <- out %>%
        dplyr::select(method, pop_size, resp_bias, resp_rate,
                      tidyselect::everything())
      return(out)
    } else {

      # If there was not a follow up, simply report the sums:
      est_mand <- function(sim_elmt){
        thvst <- sim_elmt$true_harvest[[1]]
        ests <- sim_elmt %>%
          dplyr::group_by(resp_rate) %>%
          dplyr::summarise(
            true_harvest = thvst,
            est_harvest  = sum(init_resp, na.rm = TRUE),
            est_SE       = 0L,
            ARE          = abs((est_harvest - thvst) / thvst),
            sqer         = ((est_harvest - thvst)^2),
            .groups = "keep"
          )
        return(ests)
      }

      out <- purrr::map_dfr(simdat, est_mand) %>%
        dplyr::summarise(
          method        = methods[[1]],
          pop_size      = purrr::pluck(simdat, 1, "pop_size", 1),
          resp_bias     = NA_character_,
          true_hvst     = mean(true_harvest),
          mean_hvst_est = mean(est_harvest),
          mean_SE       = mean(est_SE),
          MARE          = mean(ARE),
          RRMSE         = sqrt(mean(sqer)) / mean(true_harvest),
          .groups = "drop"
        )

      out <- out %>%
        dplyr::mutate(resp_rate = as.character(resp_rate)) %>%
        dplyr::select(method:resp_bias, resp_rate, tidyselect::everything())
      return(out)
    }

  } else if (all(methods == "simple")) {
    # SRS estimates ============================================================

    est_simp <- function(level, pop_dat){
      thvst <- pop_dat$true_harvest[[1]]
      N <- pop_dat$pop_size[[1]]

      pop <- pop_dat %>%
        dplyr::filter(dplyr::near(uns_resp_rate, level))

      init_resp_only <- pop %>%
        dplyr::filter(init_resp == 1)

      init_design <- survey::svydesign(ids   = ~1,
                                       probs = nrow(init_resp_only) / N,
                                       data  = init_resp_only,
                                       fpc   = ~pop_size)

      init_est <- survey::svytotal(~harvest, init_design)

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

          fus_est <- survey::svytotal(~harvest, fus_design)

          init_prop <- sum(pop$init_resp, na.rm = TRUE) / sum(pop$sample)
          # assume fus respondents make the rest of the pop:
          fus_prop  <- 1 - init_prop
          combined_est <- (init_est * init_prop) + (fus_est * fus_prop)

          combined_SE <- (init_prop * survey::SE(init_est)) +
            (fus_prop * survey::SE(fus_est))

          estout <- tibble::tibble(
            resp_rate    = as.character(level),
            resp_bias    = as.character(pop$resp_bias[[1]]),
            true_harvest = thvst,
            est_harvest  = as.vector(combined_est),
            est_SE       = as.vector(combined_SE),
            ARE          = abs((est_harvest - true_harvest) / true_harvest),
            sqer         = ((est_harvest - true_harvest)^2)
          )
        } else {
          estout <- tibble::tibble(
            resp_rate    = as.character(level),
            resp_bias    = as.character(pop$resp_bias[[1]]),
            true_harvest = thvst,
            est_harvest  = as.vector(init_est),
            est_SE       = as.vector(survey::SE(init_est)),
            ARE          = abs((est_harvest - true_harvest) / true_harvest),
            sqer         = ((est_harvest - true_harvest)^2)
          )
        }
      } else {
        # If there was no follow up survey simulated:
        estout <- tibble::tibble(
          resp_rate    = as.character(level),
          resp_bias    = as.character(pop$resp_bias[[1]]),
          true_harvest = thvst,
          est_harvest  = as.vector(init_est),
          est_SE       = as.vector(survey::SE(init_est)),
          ARE          = abs((est_harvest - true_harvest) / true_harvest),
          sqer         = ((est_harvest - true_harvest)^2)
        )
      }
      return(estout)
    }

    # split up single levels of response bias:
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
        MARE          = mean(ARE),
        RRMSE         = sqrt(mean(sqer)) / mean(true_harvest),
        .groups = "drop"
      )

    out <- out %>%
      dplyr::select(method, pop_size, resp_bias, resp_rate,
                    tidyselect::everything())
    return(out)

  } else if (all(methods == "voluntary")){
    # Voluntary estimates ======================================================
    est_vol <- function(level, pop_dat){
      thvst <- pop_dat$true_harvest[[1]]
      N <- pop_dat$pop_size[[1]]

      pop <- pop_dat %>%
        dplyr::filter(dplyr::near(uns_resp_rate, level))

      init_resp_only <- pop %>%
        dplyr::filter(init_resp == 1)

      init_design <- survey::svydesign(ids   = ~1,
                                       probs = nrow(init_resp_only) / N,
                                       data  = init_resp_only,
                                       fpc   = ~pop_size)

      init_est <- survey::svytotal(~harvest, init_design)

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

          fus_est <- survey::svytotal(~harvest, fus_design)

          # The prop. of respondents to self-report IS the proportion
          # of the population who reported:
          init_prop <- mean(pop$init_resp, na.rm = TRUE)
          # assume fus respondents reflect the rest of the pop.
          fus_prop  <- 1 - init_prop
          combined_est <- (init_est * init_prop) + (fus_est * fus_prop)

          combined_SE <- (init_prop * survey::SE(init_est)) +
            (fus_prop  * survey::SE(fus_est))

          estout <- tibble::tibble(
            resp_rate    = as.character(level),
            resp_bias    = as.character(pop$resp_bias[[1]]),
            true_harvest = thvst,
            est_harvest  = as.vector(combined_est),
            est_SE       = as.vector(combined_SE),
            ARE          = abs((est_harvest - true_harvest) / true_harvest),
            sqer         = ((est_harvest - true_harvest)^2)
          )
        } else {
          estout <- tibble::tibble(
            resp_rate    = as.character(level),
            resp_bias    = as.character(pop$resp_bias[[1]]),
            true_harvest = thvst,
            est_harvest  = as.vector(init_est),
            est_SE       = as.vector(survey::SE(init_est)),
            ARE          = abs((est_harvest - true_harvest) / true_harvest),
            sqer         = ((est_harvest - true_harvest)^2)
          )
        }
      } else {
        # If there was no follow up survey simulated:
        estout <- tibble::tibble(
          resp_rate    = as.character(level),
          resp_bias    = as.character(pop$resp_bias[[1]]),
          true_harvest = thvst,
          est_harvest  = as.vector(init_est),
          est_SE       = as.vector(survey::SE(init_est)),
          ARE          = abs((est_harvest - true_harvest) / true_harvest),
          sqer         = ((est_harvest - true_harvest)^2)
        )
      }
      return(estout)
    }

    # split up levels of response bias:
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
        MARE          = mean(ARE),
        RRMSE         = sqrt(mean(sqer)) / mean(true_harvest),
        .groups = "drop"
      )

    out <- out %>%
      dplyr::select(method, pop_size, resp_bias, resp_rate,
                    tidyselect::everything())
    return(out)
  } else {
    stop (paste0("'simdat' must be an unmanipulated output from either",
                 " mand(), simple(), or vol()."))
  }
}
