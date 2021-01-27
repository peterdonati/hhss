# Documentation ================================================================
#
#' Population and harvest survey simulations
#'
#' @description
#' The pop function simulates a population of hunters and whether or not
#' they were successful in harvesting.
#'
#'
#' @param n The desired population size.
#' @param split Proportion of the population that is placed into group 1.
#' The remaining proportion (i.e. \code{1 - split}) will be in group 0.
#' @param success1 Probability of a hunter in group 1 to harvest
#' @param success0 Probability of a hunter in group 0 to harvest
#'
#' @return A tibble that contains the variables:
#' \itemize{
#' \item \code{pop_size}: The population size.
#' \item \code{true_harvest}: The sum of harvests from the population.
#' \item \code{group}: The group in which the hunter was placed.
#' \item \code{harvest}: 1 for a successful hunter, and 0 if unsuccessful.
#' }
#'
#' @examples
#' # Simulate a population of 1,000 hunters and
#' # simulates harvest where each hunter has a probability of 0.3 to harvest:
#' pop(n = 1000, success1 = 0.3)
#'

# pop() ========================================================================

pop <- function(n, split = 1, success1, success0 = success1){

  argcheck <- c(split, success1, success0)

  if (any(argcheck > 1) | any(argcheck < 0)){
    stop (
      paste0("1 or more arguments that must contain proportions or",
             " probabilities are < 0 or > 1."),
      call. = FALSE
    )
  }

  if ((split != 1 & split != 0) & success0 == success1){
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

  pop <- tibble::tibble(
    pop_size = n,
    group = rbinom(n, 1, split),
    harvest = case_when(
      group == 1 ~ rbinom(n, 1, success1),
      group == 0 ~ rbinom(n, 1, success0)),
    true_harvest = sum(harvest),
  )

  pop <- dplyr::select(pop, pop_size, true_harvest, tidyselect::everything())
  return(pop)
}
