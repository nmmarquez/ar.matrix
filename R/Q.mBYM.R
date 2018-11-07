#' Modified Precision matrix for a BYM process
#'
#' @description EXPIREMENTAL. Functions for creating precision matricies and
#' observations of a modified BYM process as defined in MacNab 2011. The matrix
#' defines the precision of estimates when observations share connections which
#' are conditionally auto-regressive(CAR). Because the precision matrix is
#' not symetric the process is not a true GMRF.
#' @usage Q.mBYM(graph, sigma, rho, vcov=FALSE)
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param graph matrix, square matrix indicating where two observations are
#' connected (and therefore conditionally auto-regressive).
#' @param sigma float > 0, process standard derviation see MacNab 2011.
#' @param rho float >= 0 & < 1, how correlated neighbors are. The
#' function will still run with values outside of the range [0,1) however the
#' stability of the simulation results are not gaurunteed. see MacNab 2011.
#' @param vcov bool If the vcov matrix should be returned instead of the
#' precision matrix.
#'
#' @return Q.mBYM returns either a precision or variance-covariance function
#' with a modified BYM structure.
#'
#' r.mBYM retrurns a matrix with n rows which are the n observations of a pseudo
#' Gaussian Markov random field of a modified BYM process.
#'
#' @examples
#' 
#' \dontrun{
#' require("leaflet")
#' require("sp")
#'
#' # simulate mBYM data and attach to spatial polygons data frame
#' US.df@data$data <- c(r.mBYM(1, graph=US.graph, sigma=1, rho=.99))
#'
#' # color palette of data
#' pal <- colorNumeric(palette="YlGnBu", domain=US.df@data$data)
#'
#' # see map
#' map1<-leaflet() %>%
#'     addProviderTiles("CartoDB.Positron") %>%
#'     addPolygons(data=US.df, fillColor=~pal(data), color="#b2aeae",
#'                 fillOpacity=0.7, weight=0.3, smoothFactor=0.2) %>%
#'     addLegend("bottomright", pal=pal, values=US.df$data, title="", opacity=1)
#' map1
#'}
#'
#' @references Y.C. MacNab On Gaussian Markov random fields and Bayesian
#' disease mapping. Statistical Methods in Medical Research. 2011.
#' 
#' @export

Q.mBYM <- function(graph, sigma, rho, vcov=FALSE){
    if(sigma <= 0) stop("sigma paramter must be greater than 0.")
    D <- diag(Matrix::rowSums(graph))
    I <- diag(nrow(graph))
    Q_star <- (rho * sigma**2 * MASS::ginv(D-graph) + sigma**2 * I * (1 - rho))
    if(!vcov) Q_star <- Matrix::forceSymmetric(Matrix::solve(Q_star))
    Q_star
}
