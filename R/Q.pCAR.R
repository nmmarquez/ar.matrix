#' Precision matrix for a pCAR process
#'
#' @description Functions for creating precision matricies and observations
#' of a proper CAR(pCAR) process as defined in MacNab 2011. The matrix defines
#' the precision of estimates when observations share connections which are
#' conditionally auto-regressive(CAR).
#' @usage Q.pCAR(graph, sigma, rho, vcov=FALSE)
#'
#' @param n int > 0, number of observations to simulate from the GMRF.
#' @param graph matrix, square matrix indicating where two observations are
#' connected (and therefore conditionally auto-regressive).
#' @param sigma float > 0, pairwise observation variance
#' @param rho float >= 0 & < 1, how correlated pairwise observations are. The
#' function will still run with values outside of the range [0,1) however the
#' stability of the simulation results are not gaurunteed.
#' @param vcov bool If the vcov matrix should be returned instead of the
#' precision matrix.
#'
#' @return Q.pCAR returns either a precision or variance-covariance function
#' with a pCAR structure.
#'
#' r.pCAR retrurns a matrix with n rows which are the n observations of a
#' Gaussian Markov random field pCAR process.
#'
#' @examples
#' require("leaflet")
#' require("rgdal")
#' require("surveillance")
#' require("maptools")
#'
#' # download the 2015 shape file from us census
#' file_zip <- tempfile()
#' extr_dir <- tempdir()
#' url_ <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_county_5m.zip"
#' download.file(url_, file_zip)
#' unzip(file_zip, exdir=extr_dir)
#'
#' # pull the shape file into memory
#' df <- readOGR(extr_dir)
#' unlink(file_zip)
#' unlink(extr_dir)
#'
#' # lets only look at Louisiana(22), Texas(48), Mississippi(28), & Arkansas(05)
#' df <- df[df@data$STATEFP %in% c("22", "48", "28", "05"),]
#'
#' # translate to desired proj4 string for leaflet
#' p4s <- "+title=WGS 84 (long/lat) +proj=longlat +ellps=WGS84 +datum=WGS84"
#' df <- spTransform(df, CRS(p4s))
#'
#' # simulate data from graph derived from poly2adjmat using lCAR structure
#' df@data$data <- c(r.pCAR(1, poly2adjmat(df), 1, .99))
#'
#' # color palette of data
#' pal <- colorNumeric(palette="YlGnBu", domain=df@data$data)
#'
#' # see map
#' map1<-leaflet() %>%
#'     addProviderTiles("CartoDB.Positron") %>%
#'     addPolygons(data=df, fillColor=~pal(data), color="#b2aeae",
#'                 fillOpacity=0.7, weight=0.3, smoothFactor=0.2) %>%
#'     addLegend("bottomright", pal=pal, values=df$data, title="", opacity=1)
#' map1
#'
#' @export

Q.pCAR <- function(graph, sigma, rho, vcov=FALSE){
    if(sigma <= 0) stop("sigma paramter must be greater than 0.")
    Q <- sigma**-1 * (diag(rowSums(graph)) - rho * graph)
    if(vcov) Q <- solve(Q)
    Q
}

