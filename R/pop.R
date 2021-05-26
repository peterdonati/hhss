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
#' The remaining proportion, i.e. (\code{1 - split}), will be in group 2.
#' @param success1 Probability of a hunter in group 1 to harvest
#' @param success2 Probability of a hunter in group 2 to harvest
#'
#' @return A data frame of class \code{hhss_pop} where each observation
#' represents a hunter. It contains the variables:
#' \itemize{
#' \item \code{N}: The population size.
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
#' # group 1, and the remaining 30% are in group 2. Simulate harvest so hunters
#' # in group 1 harvest at a probability of 0.3, and hunters in group 2 at 0.5:
#' pop(N = 10000, split = 0.7, success1 = 0.3, success2 = 0.5)

# pop() ========================================================================
#' @export
pop <- function(N, split = 1, success1, success2 = success1){

  argcheck <- c(split, success1, success2)

  if (any(argcheck > 1) || any(argcheck < 0)){
    stop (
      paste0("1 or more arguments that must contain proportions or",
             " probabilities are < 0 or > 1."),
      call. = FALSE
    )
  }

  if ((split != 1 && split != 0) && success2 == success1){
    warning (
      paste0("Population split into two groups, but both groups have same",
             " probability of harvest. Consider specifying argument",
             " 'success2'."),
      call. = FALSE
    )
  }

  if ((split == 1 || split == 0) && success2 != success1){
    warning(
      paste0("Population is not 'split' into different groups, and 'success1'",
             " does not equal 'success2'. To simulate different harvest",
             " rates, the population must be 'split' into two groups."),
      call. = FALSE
    )
  }

  group <- rbinom(N, 1, split)
  harvest <- ifelse(
    group,
    rbinom(sum(group), 1, success1),
    rbinom(N - sum(group), 1, success2)
  )

  pop <- data.frame(
    N = rep(N, N),
    true_harvest = sum(harvest),
    group,
    harvest
  )

  # Re-assign group 0 to be group 2 so it's more intuitive:
  pop$group <- ifelse(pop$group, pop$group, 2L)

  pop <- pop_class(pop)
  return(pop)
}

# Helpers ======================================================================
pop_class <- function(x){
  class(x) <- c("hhss_pop", "data.frame")
  return(x)
}
