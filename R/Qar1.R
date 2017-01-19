#' Precision matrix for an AR1 process
#'
#' @description Creates a precision (or optionally variance-covariance) matrix
#' of an AR1 process
#'
#' @param N int > 0, number of obserations you want to observe
#' @param sigma float > 0, pairwise observation variance
#' @param rho float >= 0 & < 1, how correlated pairwise observations are. The
#' function will still run with values outside of the range [0,1) however the
#' stability of the simulation results are not gaurunteed.
#' @param vcov bool If the vcov matrix should be returned instead of the
#' precision matrix.
#'
#' @return Matrix object, either precision or variance covariance

Q.ar1 <- function(N, sigma, rho, vcov=FALSE){
    Q <- matrix(0, nrow=N, ncol=N)
    Q[1,1] <- 1.
    for(i in 2:N){
        Q[i,i] <- 1 + rho**2
        Q[i-1,i] <- -1 * rho
        Q[i,i-1] <- -1 * rho
    }
    Q[N,N] <- 1.
    Q <- (1 / sigma**2) * Q
    Q <- ifelse(vcov, solve(Q), Q)
    Q
}
