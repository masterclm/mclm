#' Stubs (Functions/Methods Not Implemented)
#' 
#' The \code{mclm} package contains a number of methods which don't do
#'   anything useful at all, except from producing a warning, either because useful
#'   implementations are yet to be implemented or because we simply couldn't think
#'   of a useful implementation.
#'   
#' Typical examples are (i) defaults of generic methods that are only
#'   meaningful for a limited number of classes, e.g. \code{n_tokens} and
#'   \code{n_types}, and (ii) specific instances of generic methods which target classes
#'   for which there is no implementation of the method (yet), e.g.
#'   certain instances of the plotting method \code{\link{plot}}.
#'   The methods listed on this page have in common that they
#'   don't do anything substantial
#'   (e.g. they don't return a meaningful number or they don't produce a plot)
#'   and warn the user about this.
#'   The methods listed on this page are implemented in order to prevent
#'   cryptic error messages or warning from being show when the user
#'   (accidentally) calls any of these methods.
#'   
#' @param x Some object
#' @param ... Additional arguments
#' @param i Index
#' @param value Value to change
#' 
#' @return Invisibly, \code{NULL}. The default \code{n_tokens}, \code{n_types} and
#'   \code{type_names} methods returns \code{NA}.
#' @name stubs
#' @noRd
NULL

# TODO which functions should be in this doc? How necessary is it? How much should...
# they document, if the whole point is that they are NOT supported?
