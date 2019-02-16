#' @rdname Q.RW1
#'
#' @export

r.RW1 <- function(n, M, sigma){
    Q <- Q.RW1(M, sigma)
    sim.GMRF(n, Q)
}
