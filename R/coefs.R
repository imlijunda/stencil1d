#' Calculate finite difference coefficients at artificial stencil points
#'
#' @param order order of derivative
#' @param stencil locations of sampled points
#'
#' @return a numeric vector of finite difference coefficients corresponding to stencil points.
#' @export
#'
#' @examples
#' coefs <- fd_coefs(2, -3:3)
fd_coefs <- function(order, stencil) {

  S <- t(sapply(seq.int(0L, length(stencil) - 1L), function(n) stencil^n))
  kron <- rep(0, length(stencil))
  kron[order + 1L] <- factorial(order)

  solve(S, kron)
}
