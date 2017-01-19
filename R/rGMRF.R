#' Simulate data from a GMRF
#'
#' @description Generlized function for simulating data from a Gaussian Markov
#' random field(GMRF. A string is passed in indicatting what the covariance
#' structure should be and then additional arguments are passed in based on the
#' requiremnts of the structure.
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param Qstruct string, indication of the type of covariance structure to use.
#' @param M int > 0, number of elements to observe, for AR1 process only
#' @param graph matrix, square matrix indicating where two observations are
#' connected (and therefore conditionally auto-regressive). Applicable to
#' lCAR & pCAR covariance structures.
#' @param sigma float > 0, pairwise observation variance
#' @param rho float >= 0 & < 1, how correlated pairwise observations are. The
#' function will still run with values outside of the range [0,1) however the
#' stability of the simulation results are not gaurunteed.
#' @return Matrix object, matrix where each row is a single obsrevation from
#' a GMRF with covariance structure Q^-1.
#'
#' @export

rGMRF <- function(n, Qstruct=c("Q.ar1", "Q.pCAR", "Q.lCAR"), ...){
    option <- c("Q.ar1", "Q.pCAR", "Q.lCAR")
    if(!(Qstruct[1] %in% option)){
        opt_str <- paste(option, collapse=", ")
        stop(paste0("Qstruct must be one of the following: ", opt_str))
    }
    Q <- get(Qstruct[1], envir=asNamespace("ar.matrix"))(...)
    sim.AR(n, Q)
}
