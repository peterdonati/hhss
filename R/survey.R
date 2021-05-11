# SURVEY FUNCTIONS (census, mand, simple) DOCUMENTATION AND HELPERS
# Documentation ================================================================
#' Survey Simulations
#'
#' @name survey
#'
#' @aliases mand simple vol
#'
#' @description
#' The survey functions take an output from \code{\link{pop}} and simulate
#' survey responses based on the method specified.
#' \itemize{
#'     \item \code{mand()} creates a simulation where \strong{\emph{only
#'         successful hunters are mandated to report}} and a follow up sample of
#'         non-reporters can be taken through a simple random sample.
#'     \item \code{simple()} creates a simulation where a population of hunters
#'         are surveyed using a simple random sample and a follow up survey of
#'         non-respondents from the original sample pool can be taken.
#'     \item \code{vol()} creates a simulation where \strong{\emph{all hunters
#'         report, successful or not.}} Similar to voluntary reporting,
#'         or mandatory regardless of success.
#'         Follow up surveys are completed on the non-responding portion of the
#'         population by simple random sampling.
#' }
#'
#' @details
#' More than one value can be supplied to \code{resp} and \code{bias}. These
#' functions automatically create a full factorial design on
#' these two arguments.
#' \cr\cr
#'
#' If any scaling arguments scale probabilities to be > 1, the
#' probabilities will silently be limited to 1.
#'
#' @param x An output from \code{\link{pop}}. It is the population to
#'     simulate response for.
#' @param sample Probability a hunter is sampled for a survey
#' @param resp Probability/probabilities of response.
#'     \itemize{
#'         \item In \code{simple()} and \code{vol()} it defines response
#'             probabilities for unsuccessful hunters.
#'         \item In \code{mand()} it defines response probabilities for initial
#'             reporting, and then response probabilities for unsuccessful
#'             hunters in follow up samples.
#'     }
#' @param bias Scales the rate(s) of response for successful hunters, relative
#'     to unsuccessful hunters. Introduces response bias for any value not
#'     equal to 1.
#' @param fus Logical. If \code{TRUE}, a single follow up survey will be
#'     simulated.
#' @param fus_scale Scales initial response probabilities,
#'     creating new probabilities of response for follow up surveys.
#' @param fus_sample Probability that a non-respondent is sampled for a follow
#'     up survey.
#' @param times The number of times to repeat the simulation.
#'
#' @return A list of class \code{survsim_mand}, \code{survsim_simple},
#' or \code{survsim_vol} where the length is equal to the
#' integer supplied to \code{times}. The ultimate elements are data frames
#' where each row represents a hunter. A single data frame will contain some,
#' but not all, of these variables:
#' \itemize{
#' \item \code{method}: The survey method that was used to gather responses.
#' \item \code{pop_size}: The population size.
#' \item \code{true_harvest}: The sum of harvests from the population.
#' \item \code{group}: The group in which the hunter was placed.
#' \item \code{harvest}: 1 for a successful hunter, and 0 if unsuccessful.
#' \item \code{sample}: 1 if the hunter was asked to participate in the
#' initial survey, 0 otherwise.
#' \item \code{resp_bias}: The response bias currently being simulated.
#' \item \code{resp_rate}: Only reported in \code{mand()} outputs.
#' It is the response probability for successful hunters to initially report.
#' \item \code{uns_resp_rate}: The probability at which a hunter will respond
#' to an initial survey if they were unsuccessful.
#' \item \code{suc_resp_rate}: The probability at which a hunter will respond
#' to an initial survey if they were successful.
#' \item \code{init_resp}: 1 if the hunter responded to the initial survey,
#' 0 otherwise.
#' \item \code{fus_uns_resp_rate}: The probability at which a hunter will
#' respond to a follow up survey if they were unsuccessful.
#' \item \code{fus_suc_resp_rate}: The probability at which a hunter will
#' respond to a follow up survey if they were successful.
#' \item \code{fus_sample}: 1 if the hunter was asked to participate in a
#' follow up survey, 0 otherwise.
#' \item \code{fus_resp}: 1 if they responded to the follow up survey,
#' 0 otherwise.
#' }
#'
#' @examples
#' # First, create a population:
#' my_pop <- pop(N = 1000, split = 0.7, success1 = 0.25, success0 = 0.6)
#'
#' # Simulate a simple random sample from that population:
#' simple(
#'   my_pop,
#'   sample = 0.4,
#'   resp = 0.3,
#'   bias = 1,
#'   times = 10
#'   )
#'
#' # Multiple values can be passed to 'resp' and 'bias' arguments to create
#' # simulations for each unique pairing of the two:
#' vol(
#'   my_pop,
#'   resp = seq(0.3, 0.8, 0.1),
#'   bias = c(1, 1.1, 1.2),
#'   fus = TRUE,
#'   fus_scale = 0.7,
#'   fus_sample = 0.4,
#'   times = 10
#'   )
#'
# Helpers ======================================================================

# changeto1() ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To silently change response probabilities to 1 if scaling arguments scale it
# above 1. Called by survey functions.

changeto1 <- function(x) {
  ifelse(x > 1, 1, x)
}

# multi_sim() ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This function calls a function, 'f', that is a specific single_sim_*()
# function. Each single_sim_* function iteration completes a simulation for
# a single response and bias. the multi_sim() function here calls the
# single_sim_* function MULTIPLE TIMES, once for each unique pairing of
# response rate and bias.

multi_sim <- function(r, b, f, x){
  # Find all unique pairings of resp and bias:
  combos <- tidyr::crossing(r, b)
  resp <- combos$r
  bias <- combos$b

  # r = resp rate, b = bias, f = a single_sim_* function, x = population data.
  sim_list <- purrr::map2(resp, bias, f, x)

  names(sim_list) <- paste0("resp ", resp, "; bias ", bias)
  return(sim_list)
}
