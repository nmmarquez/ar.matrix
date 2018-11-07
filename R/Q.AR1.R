#' Precision matrix for an AR1 process
#'
#' @description Functions for creating precision matricies and observations of
#' an AR1 process
#' @usage Q.AR1(M, sigma, rho, sparse=FALSE, vcov=FALSE)
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param M int > 0, number of elements in the AR1 process.
#' @param sigma float > 0, pairwise observation standard deviation.
#' @param rho float >= 0 & < 1, how correlated pairwise observations are. The
#' function will still run with values outside of the range [0,1) however the
#' stability of the simulation results are not gaurunteed.
#' @param sparse bool Should the matrix be of class 'dsCMatrix'
#' @param vcov bool If the vcov matrix should be returned instead of the
#' precision matrix.
#'
#' @return Q.AR1 returns either a precision or variance-covariance function
#' with a AR1 structure.
#'
#' r.AR1 retrurns a matrix with n rows which are the n observations of a
#' Gaussian Markov random field  AR1 process.
#'
#' @examples
#' require("ggplot2")
#' # simulate AR1 GMRF
#' obs <- r.AR1(100, M=30, sigma=1, rho=.98)
#' # resulting matrix is n x M
#' dim(obs)
#' # subtract off the first time point to more easily observe correlation
#' obs_adj <- obs - obs[,1]
#' # move objects to a data frame
#' ar1_df <- data.frame(obs=c(t(obs_adj)), realization=rep(1:100, each=30),
#'                      time=rep(1:30, 100))
#' # plot each realization
#' ggplot(data=ar1_df, aes(time, obs, group=realization, color=realization)) +
#'     geom_line()
#'
#' @export

Q.AR1 <- function(M, sigma, rho, sparse=FALSE, vcov=FALSE){
    if(sigma <= 0) stop("sigma paramter must be greater than 0.")
    Q <- Matrix::Matrix(0, nrow=M, ncol=M)
    Q[1,1] <- 1.
    for(i in 2:M){
        Q[i,i] <- 1 + rho**2
        Q[i-1,i] <- -1 * rho
        Q[i,i-1] <- -1 * rho
    }
    Q[M,M] <- 1.
    Q <- (1 / sigma**2) * Q
    if(vcov) Q <- Matrix::solve(Q)
    if(sparse) Q <- Matrix::Matrix(Q, sparse=TRUE)
    Q
}
