#' Simulate correlated data from a precision matrix.
#'
#' @description Takes in a square precision matrix, which ideally should be
#' sparse and using Choleski factorization simulates data from a mean 0 process
#' where the inverse of the precision matrix represents the variance-covariance
#' of the points in the process. The resulting simulants represent samples of a
#' Gaussian Markov random field (GMRF).
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param Q matrix, a square precision matrix.
#'
#' @return Matrix object, matrix where each row is a single obsrevation from
#' a GMRF with covariance structure Q^-1.
#'
#' @export

sim.AR <- function(n, Q){
    library(Matrix)
    library(sparseMVN)
    rmvn.sparse(n, rep(0, nrow(Q)), Cholesky(Matrix(Q, sparse=TRUE)), T)
}
