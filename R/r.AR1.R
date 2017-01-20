#' @rdname Q.AR1
#'
#' @export

r.AR1 <- function(n, M, sigma, rho){
    Q <- Q.AR1(M, sigma, rho)
    sim.AR(n, Q)
}
