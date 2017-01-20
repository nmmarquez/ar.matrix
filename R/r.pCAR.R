#' @rdname Q.pCAR
#'
#' @export

r.pCAR <- function(n, graph, sigma, rho){
    Q <- Q.pCAR(graph, sigma, rho)
    sim.AR(n, Q)
}
