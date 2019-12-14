# Utility functions for publipha package.
# Copyright (C) 2019 Jonas Moss
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
# USA.

#' Make lazy list from arguments.
#'
#' Works with passed \code{...} parameters.
#' @keywords internal
#' @param ... Parameters to put into the list.
#' @return A lazy list.
alist2 <- function(...) as.list(substitute((...)))[-1]

#' Variant of do call with that preserves argument names.
#'
#' @keywords internal
#' @param .fn Function to call.
#' @param .args List of arguments to \code{.fn}.
#' @param ... Further arguments to \code{.fn}.
#' @param .env The environment where the call is to be evaluated.
#' @return The effect of calling \code{.fn} with the supplied arguments in the
#' specified environment.
do_call <- function(.fn, .args = NULL, ..., .env = parent.frame()) {
  eval(as.call(c(.fn, .args, alist2(...))), envir = .env)
}

#' Adds named elements to a list when they are not there already.
#'
#' @keywords internal
#' @param input List. The input list to manipulate.
#' @param ... Key value pairs to add to the list provided the key is not already
#' used.
#' @param .eager Logical; Should the \code{value}s be evaluated?
#' @return A modified list.
add_elements <- function(input, ..., .eager = TRUE) {
  dots <- if (.eager) list(...) else alist2(...)
  names <- names(dots)
  n <- length(names)

  for (i in 1:n) if (is.null(input[[names[i]]])) input[[names[i]]] <- dots[[i]]

  input
}

#' Get arguments of the calling function.
#'
#' @keywords internal
#' @param expand_dots Logical; If \code{TRUE}, returns the argument list with
#'     expanded \code{...}.
#'
#' @return The unevaluated argument list given to the calling function.
arguments <- function(expand_dots = FALSE) {
  formals <- names(formals(sys.function(-1)))
  reduced_formals <- setdiff(formals, "...")
  arguments <- as.list(match.call(sys.function(-1), sys.call(-1))[-1])
  if (!expand_dots) arguments[reduced_formals] else arguments
}
