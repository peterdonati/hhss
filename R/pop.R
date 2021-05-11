# Documentation ================================================================
#
#' Population and Harvest Simulations
#'
#' @description
#' The pop function simulates a population of hunters and whether or not
#' they were successful in harvesting.
#'
#'
#' @param N The desired population size.
#' @param split Proportion of the population that is placed into group 1.
#' The remaining proportion (i.e. \code{1 - split}) will be in group 0.
#' @param success1 Probability of a hunter in group 1 to harvest
#' @param success0 Probability of a hunter in group 0 to harvest
#'
#' @return A data frame of class \code{hhss_pop} that contains the variables:
#' \itemize{
#' \item \code{pop_size}: The population size.
#' \item \code{true_harvest}: The sum of harvests from the population.
#' \item \code{group}: The group in which the hunter was placed.
#' \item \code{harvest}: 1 for a successful hunter, and 0 if unsuccessful.
#' }
#'
#' @examples
#' # Simulate a population of 10,000 hunters where each hunter has a
#' # probability of 0.3 to harvest:
#' pop(N = 10000, success1 = 0.3)
#'
#' # Simulate a population of 10,000 hunters where roughly 70% of hunters are in
#' # group 1, and the remaining 30% are in group 0. Simulate harvest so hunters
#' # in group 1 harvest at a probability of 0.3, and hunters in group 0 at 0.5:
#' pop(N = 10000, split = 0.7, success1 = 0.3, success0 = 0.5)

# pop() ========================================================================
#' @export
pop <- function(N, split = 1, success1, success0 = success1){

  argcheck <- c(split, success1, success0)

  if (any(argcheck > 1) || any(argcheck < 0)){
    stop (
      paste0("1 or more arguments that must contain proportions or",
             " probabilities are < 0 or > 1."),
      call. = FALSE
    )
  }

  if ((split != 1 && split != 0) && success0 == success1){
    warning (
      paste0("Population split into two groups, but both groups have same",
             " probability of harvest. Consider specifying argument",
             " 'success0'."),
      call. = FALSE
    )
  }

  if ((split == 1 || split == 0) && success0 != success1){
    warning(
      paste0("Population is not 'split' into different groups, and 'success1'",
             " does not equal 'success0'. To simulate different harvest",
             " rates, the population must be 'split' into two groups."),
      call. = FALSE
    )
  }

  pop <- tibble::tibble(
    pop_size = N,
    group = rbinom(N, 1, split),
    harvest = ifelse(group, rbinom(N, 1, success1), rbinom(N, 1, success0)),
    true_harvest = sum(harvest),
  )

  pop <- dplyr::select(pop, pop_size, true_harvest, tidyselect::everything())
  pop <- pop_class(pop)
  return(pop)
}

# Helpers ======================================================================
pop_class <- function(x){
  class(x) <- c("hhss_pop", "data.frame")
  return(x)
}
