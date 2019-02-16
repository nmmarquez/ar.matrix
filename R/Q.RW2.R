#' Precision matrix for an RW2 process
#'
#' @description Functions for creating precision matricies and observations of
#' an RW2 process
#' @usage Q.RW2(M, sigma, sparse=TRUE)
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param M int > 0, number of elements in the RW2 process.
#' @param sigma float > 0, pairwise observation standard deviation.
#' @param sparse bool Should the matrix be of class 'dsCMatrix'
#'
#' @return Q.RW2 returns a precision matrix with a RW2 structure.
#'
#' r.RW2 retrurns a matrix with n rows which are the n observations of an
#' Intrinsic Gaussian Markov random field  RW2 process.
#'
#' @examples
#' require("ggplot2")
#' # simulate RW2 GMRF
#' obs <- r.RW2(100, M=30, sigma=1)
#' # resulting matrix is n x M
#' dim(obs)
#' # move objects to a data frame
#' RW2_df <- data.frame(obs=c(t(obs)), realization=rep(1:100, each=30),
#'                      time=rep(1:30, 100))
#' # plot each realization
#' ggplot(data=RW2_df, aes(time, obs, group=realization, color=realization)) +
#'     geom_line()
#'
#' @export

Q.RW2 <- function(M, sigma, sparse=TRUE){
    if(sigma <= 0) stop("sigma paramter must be greater than 0.")
    Q <- Matrix::Matrix(0, nrow = M, ncol = M)
    Q[1, 1:3] <- c(1, -2, 1)
    Q[2, 1:4] <- c(-2, 5, -4, 1)
    for (i in 3:(M-2)){
        Q[i, (i-2):(i+2)] <- c(1, -4, 6, -4, 1)
    }
    Q[M-1, (M-3):M] <- c(1, -4, 5, -2)
    Q[M, (M-2):M] <- c(1, -2, 1)
    if(!sparse)
        Q <- Matrix::Matrix(Q, sparse = FALSE)
    (1/sigma^2) * Q
}
