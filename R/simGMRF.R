#' Simulate correlated data from a precision matrix.
#'
#' @description Takes in a square precision matrix of a gaussian Markov random 
#' field, which ideally should be sparse, and using algorithm 3.1 from Gaussian
#' Markov Random Fields by Rue & Held simulates data from a mean 0 process
#' where the inverse of the precision matrix represents the variance-covariance
#' of the points in the process. The resulting simulants represent samples of a
#' Gaussian Markov random field (GMRF).
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param Q matrix, a square precision matrix of a GMRF.
#' @param tol numeric, The tolerance for the removal of zero eigenvalues.
#'
#' @return Matrix object, matrix where each row is a single obsrevation from
#' a GMRF with precision structure Q.
#'
#' @examples
#' require("ggplot2")
#'
#' # simulate 2D rw1 process
#' # pairwise variance
#' sigma <- .5
#'
#' # 2 dimensions of simulations
#' years <- 20
#' ages <- 10
#'
#' # kronnecker product to get joint precision
#' Q2D <- kronecker(Q.RW1(M=years, sigma), Q.RW1(M=ages, sigma))
#'
#' # simulate the data and place it in a data frame
#' Q2D.df <- data.frame(obs=c(sim.GMRF(1, Q2D)), age=rep(1:ages, years),
#'                      year=rep(1:years, each=ages))
#'
#' # graph results
#' ggplot(data=Q2D.df, aes(year, obs, group=age, color=age)) + geom_line()
#'
#' @references Rue, H. Held, L. Gaussian Markov Random Fields: Theory and 
#' Applications. Chapman and Hall. 2005.
#' @export

sim.GMRF <- function(n, Q, tol=1e-12){
    Es <- eigen(Q)
    LAMBDAS <- rev(Es$values)
    ai <- LAMBDAS > tol
    Vtilde <- Es$vectors[,sum(ai):1]
    
    yij <- matrix(
        stats::rnorm(sum(ai) * n, 0, sqrt(rep(LAMBDAS[ai], n)^-1)),
        nrow=sum(ai),
        ncol=n)
    
    t(Vtilde %*% yij)
}
