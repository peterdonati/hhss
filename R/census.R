# census() ========================================================================
#' @rdname survey
#' @export

census <- function(x, resp, bias, fus = FALSE,
                fus_sample = NULL, fus_scale = NULL, times = 1) {

  #Argument checks ----
  if (!inherits(x, "hhss_pop")){
    stop ("'x' not of class 'hhss_pop'")
  }

  if (times %% 1 != 0){
    stop ("'times' must be a whole number.", call. = FALSE)
  }

  argcheck <- c(fus_sample, resp)
  if (any(argcheck > 1) || any(argcheck <= 0)){
    stop (
      "'resp' and 'fus_sample' must be proportions and > 0.",
      call. = FALSE
    )
  }

  if (fus && (missing(fus_scale) || missing(fus_sample))){
    stop (
      "If 'fus' = TRUE, 'fus_scale' and 'fus_sample' arguments
      must be defined.",
      call. = FALSE
    )
  }

  if (!fus && (!missing(fus_scale) || !missing(fus_sample))){
    stop ("'fus_scale' and/or 'fus_sample' are defined, but 'fus' = FALSE.",
          call. = FALSE)
  }

  if (!missing(fus_scale) && fus_scale > 1){
    message(
      "fus_scale > 1; Hunters more likely to respond to follow up
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
  N <- x$pop_size[[1]]

  single_sim_v <- function(.r, .b, dat){
    out <- dplyr::mutate(
      dat,
      method = "census",
      resp_bias = .b,
      uns_resp_rate = .r,
      suc_resp_rate = .r * .b,
      suc_resp_rate = changeto1(suc_resp_rate),
      init_resp = ifelse(
        harvest,
        rbinom(N, 1, suc_resp_rate),
        rbinom(N, 1, uns_resp_rate)
      )
    )

    if (fus){
      out <- dplyr::mutate(
        out,
        fus_uns_resp_rate = uns_resp_rate * fus_scale,
        fus_suc_resp_rate = suc_resp_rate * fus_scale,
        fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
        fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
        fus_sample = ifelse(init_resp, NA_integer_, rbinom(N, 1, fus_sample)),
        fus_resp =
          dplyr::case_when(
            fus_sample == 0 ~ NA_integer_,
            harvest == 1 ~ rbinom(N, 1, fus_suc_resp_rate),
            harvest == 0 ~ rbinom(N, 1, fus_uns_resp_rate)
          )
      )
    }

    out <- dplyr::select(
      out, method, pop_size, true_harvest, tidyselect::everything()
    )

    return(out)
  }

  out <- vector(mode = "list", length = times)
  for(i in 1:times){
    out[[i]] <- multi_sim(resp, bias, single_sim_v, x)
  }

  names(out) <- paste("Sim", 1:length(out))
  out <- survsim_census_class(out)
  return(out)
}

# Helpers ======================================================================
survsim_census_class <- function(x){
  class(x) <- "survsim_census"
  return(x)
}
