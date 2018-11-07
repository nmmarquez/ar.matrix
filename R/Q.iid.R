#' Precision matrix for a IID process
#'
#' @description Functions for creating precision matricies and observations
#' of a independent identically distributed GMRF process.
#' @usage Q.iid(M, sigma, sparse=FALSE,  vcov=FALSE)
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param M int > 0, number of elements in the process.
#' @param sigma float > 0, standard deviat
#' @param sparse bool Should the matrix be of class 'dsCMatrix'
#' @param vcov bool If the vcov matrix should be returned instead of the
#' precision matrix.
#'
#' @return Q.iid returns either a precision or variance-covariance function
#' with iid structure.
#'
#' r.iid retrurns a matrix with n rows which are the n observations of a
#' Gaussian Markov random field iid process.
#'
#' @examples
#' require("leaflet")
#' require("sp")
#'
#' # simulate iid data and attach to spatial polygons data frame
#' US.df@data$data <- c(r.iid(1, M=nrow(US.graph), sigma=1))
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
#'
#' @export

Q.iid <- function(M, sigma, sparse=FALSE, vcov=FALSE){
    if(sigma <= 0) stop("sigma paramter must be greater than 0.")
    Q <- diag(M) * 1/sigma**2
    if(vcov) Q <- Matrix::solve(Q)
    if(sparse) Q <- Matrix::Matrix(Q, sparse=TRUE)
    Q
}
