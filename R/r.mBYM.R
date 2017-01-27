#' @rdname Q.mBYM
#'
#' @export

r.mBYM <- function(n, graph, sigma, rho){
    Q_star <- Q.mBYM(graph, sigma, rho)
    sim.AR(n, Q_star)
}
