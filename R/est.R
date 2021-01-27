# Documentation ================================================================
#'
#' Estimate total harvest
#'
#' @description Estimates total harvest from a data set created by one of
#' the \code{\link{survey}} functions.
#'
#' @param simdat Simulation data. An output from either \code{simple()},
#' \code{mand()}, or \code{vol()}.
#'
#' @details
#' This function relies on the structure of \code{simdat} being unchanged from
#' its original structure to know how to create estimates.
#' If you must manipulate the data set, create estimates
#' from it first, then manipulate it.\cr\cr
#'
#' If estimates are to be made from a data set that contains multiple response
#' simulations, with multiple response biases and/or multiple response rates
#' within each simulation, this function breaks the data set down into
#' single populations and creates an estimate for every single population.
#' It then returns the averages for these estimates.\cr\cr
#'
#' All estimations use \code{survey::svydesign()} and
#' \code{survey::svytotal()} to estimate total harvest. Estimates are
#' calculated a bit different when a follow up survey is simulated in the
#' data. In that case, two separate estimates are made using
#' \code{survey::svytotal()}. One estimate from the respondents to
#' the initial survey and the other from respondents
#' to follow up surveys.\cr\cr
#'
#' In the case of estimates from \code{simple()} outputs,
#' the proportion of (hunters initially responding / hunters sampled) is
#' used to scale the initial estimate (i.e. the prop. of respondents is
#' assumed to make up the same
#' prop. of the entire population). Then, the follow up respondents are assumed
#' to be representative of the remaining proportion of the entire population.
#' This proportion is used to scale the follow up estimate.
#' Both scaled estimates are then added together to create the combined
#' estimate.\cr\cr
#'
#' This is similar to estimates made from \code{vol()} outputs,
#' except in that case, the proportion of initial respondents \emph{is} the
#' proportion of the population that responded.\cr\cr
#'
#' Estimates for \code{mand()} outputs assume 100\% reporting by successful
#' hunters for initial reports. If a follow up survey was simulated, it creates
#' a harvest estimate from the follow up sample to estimate total harvest for
#' the non-reporting portion of the population, and then adds that to the
#' sum of initially reported harvests.\cr\cr
#'
#' @return A tibble, containing the following variables:
#' \itemize{
#' \item \code{method}: The original survey method.
#' \item \code{pop_size}: Hunter population size.
#' \item \code{resp_bias}: Bias of successful hunters to report,
#' relative to unsuccessful hunters.
#' \item \code{resp_rate}: Underlying response probabilities of hunters, if
#' there was no bias.
#' \item \code{mean_true_hvst}:
#' \item \code{}
#' }
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

  simdat <- purrr::flatten(simdat)
  methods <- purrr::map_chr(simdat, ~.x$method[[1]])


  if (all(methods == "mandatory")){
    # Mandatory estimates ======================================================

    if ("fus_resp" %in% names(simdat[[1]])){

      est_mand <- function(pop_dat){

        thvst <- pop_dat$true_harvest[[1]]
        N <- pop_dat$pop_size[[1]]

        # Initial estimate will just be sum of reports:
        init_est <- sum(pop_dat$init_resp)

        # Separate estimate of non-respondents:
        fus_resp_only <- dplyr::filter(pop_dat, fus_resp == 1)

        fus_resp_only <- dplyr::mutate(
          fus_resp_only,
          fpc = N - sum(pop_dat$init_resp)
        )

        fus_design <- survey::svydesign(ids = ~1,
                                        probs = nrow(fus_resp_only) /
                                          (N - sum(pop_dat$init_resp)),
                                        data = fus_resp_only,
                                        fpc = ~fpc)

        fus_est <- survey::svytotal(~harvest, fus_design)

        combined_est <- init_est + fus_est

        estout <- tibble::tibble(
          resp_rate = pop_dat$resp_rate[[1]],
          resp_bias = pop_dat$resp_bias[[1]],
          true_harvest = thvst,
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
          est_harvest = sum(pop_dat$init_resp, na.rm = TRUE),
          est_SE = 0L, # Assuming 100% reporting
          ARE = abs((est_harvest - true_harvest) / true_harvest),
          sqer = ((est_harvest - true_harvest)^2)
        )

        return(estout)
      }
    }

    # Use map() to pull individual dataframes from simdat list
    # and make individual estimates:
    ests <- purrr::map_dfr(simdat, est_mand)

    out <- output_summarizer(ests = ests)
    return(out)

  } else if (all(methods == "simple")) {
    # SRS estimates ============================================================

    est_simp <- function(pop_dat){

      thvst <- pop_dat$true_harvest[[1]]
      N <- pop_dat$pop_size[[1]]

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

    out <- output_summarizer(ests = ests)
    return(out)

  } else if (all(methods == "voluntary")){
    # Voluntary estimates ======================================================

    est_vol <- function(pop_dat){

      thvst <- pop_dat$true_harvest[[1]]
      N <- pop_dat$pop_size[[1]]

      init_resp_only <- dplyr::filter(pop_dat, init_resp == 1)

      init_design <- survey::svydesign(ids = ~1,
                                       probs = nrow(init_resp_only) / N,
                                       data = init_resp_only,
                                       fpc = ~pop_size)

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
    out <- output_summarizer(ests = ests)
    return(out)

  } else {
    stop (paste0("'simdat' must be an unmanipulated output from either",
                 " mand(), simple(), or vol()."))
  }
}
