#' @rdname Q.AR1
#'
#' @export

r.AR1 <- function(n, M, sigma, rho){
    Q <- Q.ar1(graph, sigma, rho)
    sim.AR(n, Q)
}
