#' Precision matrix for an RW1 process
#'
#' @description Functions for creating precision matricies and observations of
#' an RW1 process
#' @usage Q.RW1(M, sigma, sparse=TRUE)
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param M int > 0, number of elements in the RW1 process.
#' @param sigma float > 0, pairwise observation standard deviation.
#' @param sparse bool Should the matrix be of class 'dsCMatrix'
#'
#' @return Q.RW1 returns a precision matrix with a RW1 structure.
#'
#' r.RW1 retrurns a matrix with n rows which are the n observations of an
#' Intrinsic Gaussian Markov random field  RW1 process.
#'
#' @examples
#' require("ggplot2")
#' # simulate RW1 GMRF
#' obs <- r.RW1(100, M=30, sigma=1)
#' # resulting matrix is n x M
#' dim(obs)
#' # subtract off the first time point to more easily observe correlation
#' obs_adj <- obs - obs[,1]
#' # move objects to a data frame
#' rw1_df <- data.frame(obs=c(t(obs_adj)), realization=rep(1:100, each=30),
#'                      time=rep(1:30, 100))
#' # plot each realization
#' ggplot(data=rw1_df, aes(time, obs, group=realization, color=realization)) +
#'     geom_line()
#'
#' @export

Q.RW1 <- function(M, sigma, sparse=TRUE){
    if(sigma <= 0) stop("sigma paramter must be greater than 0.")
    Q <- Matrix::Matrix(0, nrow=M, ncol=M)
    Q[1,1] <- 1.
    for(i in 2:M){
        Q[i,i] <- 2
        Q[i-1,i] <- -1 
        Q[i,i-1] <- -1
    }
    Q[M,M] <- 1.
    Q <- (1 / sigma**2) * Q
    if(!sparse) Q <- Matrix::Matrix(Q, sparse=FALSE)
    Q
}
