#' Precision matrix for a pCAR process
#'
#' @description Creates a precision (or optionally variance-covariance) matrix
#' of a proper CAR(pCAR) process as defined in MacNab 2011. The matrix defines
#' the precision of estimates when observations share connections which are
#' conditionally auto-regressive(CAR).
#'
#' @param graph matrix, square matrix indicating where two observations are
#' connected (and therefore conditionally auto-regressive).
#' @param sigma float > 0, pairwise observation variance
#' @param rho float >= 0 & < 1, how correlated pairwise observations are. The
#' function will still run with values outside of the range [0,1) however the
#' stability of the simulation results are not gaurunteed.
#' @param vcov bool If the vcov matrix should be returned instead of the
#' precision matrix.
#'
#' @return Matrix object, either precision or variance covariance

Q.pCAR <- function(graph, sigma, rho, vcov=FALSE){
    Q <- sigma**-1 * (diag(rowSums(graph)) - rho * graph)
    Q <- ifelse(vcov, solve(Q), Q)
    Q
}
