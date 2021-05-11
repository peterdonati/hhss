# mand() =======================================================================
#' @rdname survey
#' @export

mand <- function(x, resp, fus = FALSE, bias = NULL,
                 fus_sample = NULL, fus_scale = NULL, times = 1){

  # Argument checks ----
  if (!inherits(x, "hhss_pop")){
    stop ("'x' not of class 'hhss_pop'")
  }

  if (times %% 1 != 0){
    stop ("'times' must be a whole number.", call. = FALSE)
  }

  argcheck <- c(resp, fus_sample)
  if (any(argcheck > 1) || any(argcheck <= 0)) {
    stop("'resp' and/or 'fus_sample' must be proportions and > 0.",
         call. = FALSE)
  }

  if (fus){
    if (missing(fus_scale)) {
      stop("If 'fus' = TRUE, 'fus_scale' argument must be defined.",
           call. = FALSE)
    }
    if (missing(fus_sample)) {
      stop("If 'fus' = TRUE, 'fus_sample' argument must be defined.",
           call. = FALSE)
    }
    if (missing(bias)) {
      stop("If 'fus' = TRUE, 'bias' argument must be defined.",
           call. = FALSE)
    }
  }

  if (!fus){
    if (!missing(fus_scale)) {
      stop("'fus_scale' is defined, but 'fus' = FALSE.", call. = FALSE)
    }
    if (!missing(fus_sample)) {
      stop("'fus_sample' is defined, but 'fus' = FALSE.", call. = FALSE)
    }
    if (!missing(bias)) {
      stop(
        "'bias' is defined, but 'fus' = FALSE.
        Response bias is only simulated in follow up survey.",
        call. = FALSE
      )
    }
  }

  if (!missing(fus_scale) && fus_scale > 1) {
    message(
      "fus_scale > 1; Hunters more likely to respond to follow up
        than to initial survey."
    )
  }

  # Actual function ----
  x <- as.data.frame(x)

  if (!fus){
    N <- x$pop_size[[1]]
    bias <- rep("NA", times = length(resp))

    # Later passed into a map2() function that creates a list with a new
    # element for each response scenario.
    single_sim_m1 <- function(.r, dat){
      dat <- dplyr::mutate(
        dat,
        method = "mand",
        resp_rate = .r,
        init_resp = ifelse(harvest, rbinom(N, 1, resp_rate), 0L
          )
      )

      dat <- dplyr::select(
        dat, method, pop_size, true_harvest, tidyselect::everything()
      )
    }

    # This scenario needs its own multi_sim function because there is no
    # reporting bias:
    multi_sim_m1 <- function(){
      sim_list <- purrr::map(resp, single_sim_m1, x)

      names(sim_list) <- paste0("resp ", resp, "; bias NA")
      return(sim_list)
    }

    out <- vector(mode = "list", length = times)
    for(i in 1:times){
      out[[i]] <- multi_sim_m1()
    }
    names(out) <- paste("Sim", 1:length(out))
    out <- survsim_mand_class(out)
    return(out)

  } else if (fus) {

    N <- x$pop_size[[1]]

    single_sim_m2 <- function(.r, .b, dat){
      dat <- dplyr::mutate(
        dat,
        method = "mand",
        resp_bias = .b,
        resp_rate = .r,
        init_resp = ifelse(harvest, rbinom(N, 1, resp_rate), 0L
        ),
        fus_sample = ifelse(init_resp, NA_integer_, rbinom(N, 1, fus_sample)),
        fus_uns_resp_rate = resp_rate * fus_scale,
        fus_uns_resp_rate = changeto1(fus_uns_resp_rate),
        fus_suc_resp_rate = resp_rate * resp_bias * fus_scale,
        fus_suc_resp_rate = changeto1(fus_suc_resp_rate),
        fus_resp = dplyr::case_when(
          init_resp == 1 ~ NA_integer_,
          fus_sample == 0 ~ NA_integer_,
          harvest == 1 ~ rbinom(N, 1, fus_suc_resp_rate),
          harvest == 0 ~ rbinom(N, 1, fus_uns_resp_rate)
        )
      )

      dat <- dplyr::select(
        dat, method, pop_size, true_harvest, tidyselect::everything()
      )
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

# Helpers ======================================================================
survsim_mand_class <- function(x){
  class(x) <- "survsim_mand"
  return(x)
}
