#' @rdname Q.iid
#'
#' @export

r.iid <- function(n, M, sigma){
    Q <- Q.iid(M, sigma)
    sim.AR(n, Q)
}
