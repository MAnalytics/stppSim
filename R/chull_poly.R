#' @title Generate boundary around a Set of Points
#' @description Generate a boundary (polygon) around
#' a set of point samples, using Convex Hull technique.
#' @param xycoords (dataframe) coordinate vectors of points.
#' A 2-column matrix or list: `x` - the eastings,
#' and `y` - the northing.
#' @usage chull_poly (xycoords)
#' @examples
#' @details The algorithm is that given by Eddy (1977).
#' @references Eddy, W. F. (1977).
#' A new convex hull algorithm for planar sets.
#' ACM Transactions on Mathematical Software,
#' 3, 398--403.10.1145/355759.355766.
#' @importFrom dplyr select
#' @export
stp_learner <- function(xycoords){

  set.seed(1)
  dat <- matrix(stats::rnorm(2000), ncol = 2)
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ]  # closed polygon

  plot(dat, pch=19)
  lines(coords, col="red")

  set.seed(1)
  dat <- matrix(stats::rnorm(2000), ncol = 2)
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ]  # closed polygon

  plot(dat, pch=19)
  lines(coords, col="red")


  Computes the subset of points which lie on the convex hull of the set of points specified.


}
