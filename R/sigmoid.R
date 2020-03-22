#' Sigmoid function
#'
#' A simple function for obtaining \eqn{f(x) = 1/(1+exp(-x))}.
#'
#' @param x numeric vector
#'
#' @return a numeric
#' @export
#'
#' @examples
#' sigmoid(c(-10, -1, 0, 1, 10))
sigmoid <- function(x){
    1/(1+exp(-x))
}