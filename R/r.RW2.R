#' @rdname Q.RW2
#'
#' @export

r.RW2 <- function(n, M, sigma){
    Q <- Q.RW2(M, sigma)
    sim.GMRF(n, Q)
}
