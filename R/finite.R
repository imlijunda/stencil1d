#' Calculate finite difference of a function
#'
#' @param f a function or a numeric vector
#' @param x position to calculate the finite difference
#' @param order order of derivative
#' @param stencil sampled points relative to x
#' @param h an epsilon
#'
#' @return a numeric value, the derivative of f(x)
#' @export
#'
finite_diff <- function(f, x, order, stencil, h = 1.0) {

  coefs <- fd_coefs(order, stencil)
  if (is.function(f)) {
    sampled <- sapply(seq_along(stencil), function(i) {
      coefs[i] * f(x + stencil[i] * h)
    })
  } else {
    sampled <- sapply(seq_along(stencil), function(i) {
      #TODO: interpolate if h is not integer
      coefs[i] * f[x + stencil[i] * h]
    })
  }

  sum(sampled) / h^2.0
}
