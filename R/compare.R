#' @title Compare two spatiotemporal point patterns
#' @description Generates series of visualizations
#' comparing two spatiotempora point patterns
#' @param stp1 (list or dataframe) First spatio-temporal
#' data containing three
#' basic columns: `x` - eastings,
#' `y` - northing, and `t` - time of occurrence
#' (in the format: `yyyy-mm-dd').
#' @param stp2 (list or dataframe) Second spatio-temporal
#' data containing three
#' basic columns: `x` - eastings,
#' `y` - northing, and `t` - time of occurrence
#' (in the format: `yyyy-mm-dd').
#' @param dim (string) The dimension of the datasets
#' to compare. Default: \code{spatial}. Other available
#' options are: \code{temporal} and \code{spatiotemporal}.
#' @usage compare(stp1, stp2, dim = "spatial")
#' @examples
#' @details
#' @return Returns series of graphs and statistics
#' describing the similarities between two spatiotemporal
#' datasets.
#' @references https://www.google.co.uk/
#' @export
compare <- function(stp1, stp2, dim = "spatial"){

}
