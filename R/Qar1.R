#' Precision matrix for an AR1 process
#'
#' @description Functions for creating precision matricies and observations of
#' an AR1 process
#' @usage Q.AR1(M, sigma, rho, vcov=FALSE)
#' r.AR1(n, M, sigma, rho)
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param M int > 0, number of elements in the AR1 process.
#' @param sigma float > 0, pairwise observation variance.
#' @param rho float >= 0 & < 1, how correlated pairwise observations are. The
#' function will still run with values outside of the range [0,1) however the
#' stability of the simulation results are not gaurunteed.
#' @param vcov bool If the vcov matrix should be returned instead of the
#' precision matrix.
#'
#' @return Q.AR1 returns either a precision or variance-covariance function
#' with a AR1 structure.
#'
#' r.AR1 retrurns a matrix with n rows which are the n observations of a
#' Gaussian Markov random field  AR1 process.
#'
#' @export

Q.AR1 <- function(M, sigma, rho, vcov=FALSE){
    Q <- matrix(0, nrow=M, ncol=M)
    Q[1,1] <- 1.
    for(i in 2:M){
        Q[i,i] <- 1 + rho**2
        Q[i-1,i] <- -1 * rho
        Q[i,i-1] <- -1 * rho
    }
    Q[M,M] <- 1.
    Q <- (1 / sigma**2) * Q
    if(vcov) Q <- solve(Q)
    Q
}

r.AR1 <- function(n, M, sigma, rho){
    Q <- Q.ar1(graph, sigma, rho)
    sim.AR(n, Q)
}
