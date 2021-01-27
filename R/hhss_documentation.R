#' hhss: Hunter Harvest survey simulations, and estimations of harvest.
#'
#' There are three types of functions within this package:
#' \code{\link{pop}},
#' \code{\link{survey}},
#' \code{\link{est}} \cr\cr
#'
#' The general flow is designed to go:\cr
#' \enumerate{
#'     \item Make a population with \code{pop()}\cr
#'     \item Take that population and put it through multiple simulations using
#'         the \code{survey} functions\cr
#'     \item Estimate total harvest from those simulations using \code{est()}
#'  }
#'
#'
#' @section pop function:
#' Simulate a population of hunters and their harvest outcomes.
#'
#' @section survey functions:
#' Simulate response to different survey methods.
#'
#' @section est function:
#' Make total harvest estimates from simulated data created by
#' survey functions.
#'
#' @docType package
#' @name hhss
NULL
