################################################################################
# HELPER FUNCTIONS
#
# For hhss package. These functions are not to be called on their own,
# but are rather used within other functions of the package.
################################################################################


# changeto1() ==================================================================

# To silently change response probabilities to 1 if scaling arguments scale it
# above 1. Called by survey functions.

changeto1 <- function(x) {
  dplyr::case_when(
    x > 1 ~ 1,
    TRUE ~ x
  )
}


# multi_sim() ==================================================================

# Creates a multi-level list, where the top level is a new
# simulation for each 'time', and the inside level is split by simulations
# for each unique response rate by using function defined by 'f'. Called by
# survey functions

multi_sim <- function(x, resp, bias, f){
  # Find all unique pairings of resp and bias:
  combos <- tidyr::crossing(resp, bias)
  resp <- combos$resp
  bias <- combos$bias

  sim_list <- purrr::map(1:nrow(combos), ~x)
  sim_list <- purrr::pmap(list(sim_list, resp, bias), f)

  names(sim_list) <- paste0("resp ", resp, "; bias ", bias)
  return(sim_list)
}


# output_summarizer() ==========================================================

# Groups individual harvest estimate outputs and reports findings. This creates
# a consistent output for all possible scenarios. Called by the est function.

output_summarizer <- function(ests){
  # Group, so avgs for each repetition of the same resp and bias can be made
  ests <- dplyr::group_by(ests, resp_bias, resp_rate)

  out <- dplyr::summarise(
    ests,
    pop_size = simdat[[1]]$pop_size[[1]],
    true_hvst = mean(true_harvest),
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
