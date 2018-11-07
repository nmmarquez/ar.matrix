#' Spatial Polygons Data Frame of Counties for Several States
#'
#' @name US.df
#' @description Spatial Polygons data frame with 475 counties from the US states
#' Louisiana, Texas, Mississippi, & Arkansas. FIPS codes for the state and
#' county are provided in the data frame.
#' @docType data
#' @keywords data
NULL

#' Matrix of Shared Boundaries Between US.df Counties
#'
#' @name US.graph
#' @description A 475x475 matrix where the index corresponds to a row in the
#' US.df Spatial Polygons data frame and the index of the matrix at row i column
#' j is 1 when US.df[i,] and US.df[j,] share a border and 0 when they do not.
#' @docType data
#' @keywords data
NULL