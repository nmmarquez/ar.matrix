#' @rdname Q.lCAR
#'
#' @export

r.lCAR <- function(n, graph, sigma, rho){
    Q <- Q.lCAR(graph, sigma, rho)
    sim.AR(n, Q)
}
