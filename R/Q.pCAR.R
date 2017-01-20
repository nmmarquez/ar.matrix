#' Precision matrix for a pCAR process
#'
#' @description Functions for creating precision matricies and observations
#' of a proper CAR(pCAR) process as defined in MacNab 2011. The matrix defines
#' the precision of estimates when observations share connections which are
#' conditionally auto-regressive(CAR).
#' @usage Q.pCAR(graph, sigma, rho, vcov=FALSE)
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param graph matrix, square matrix indicating where two observations are
#' connected (and therefore conditionally auto-regressive).
#' @param sigma float > 0, pairwise observation variance
#' @param rho float >= 0 & < 1, how correlated pairwise observations are. The
#' function will still run with values outside of the range [0,1) however the
#' stability of the simulation results are not gaurunteed.
#' @param vcov bool If the vcov matrix should be returned instead of the
#' precision matrix.
#'
#' @return Q.pCAR returns either a precision or variance-covariance function
#' with a pCAR structure.
#'
#' r.pCAR retrurns a matrix with n rows which are the n observations of a
#' Gaussian Markov random field pCAR process.
#'
#' @export

Q.pCAR <- function(graph, sigma, rho, vcov=FALSE){
    Q <- sigma**-1 * (diag(rowSums(graph)) - rho * graph)
    if(vcov) Q <- solve(Q)
    Q
}

r.pCAR <- function(n, graph, sigma, rho){
    Q <- Q.pCAR(graph, sigma, rho)
    sim.AR(n, Q)
}