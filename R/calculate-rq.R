#' Calculate RQ from ddCT data.
#'
#' Calculate relative quantification (RQ) from delta delta CT.
#'
#' @param ddCT ddCT qPCR data as a \code{data.table} (i.e., output of \code{calculate_ddct()}).
#' @param genes A character vector listing genes corresponding to a subset of the columns of each \code{data.table} object.
#' @return Relative quantification (RQ) data as a \code{data.table}.
#' @author Ayush Noori
#' @export
calculate_rq = function(ddCT, genes) {

  # calculate RQ
  message("Calculating relative quantification.")
  my_RQ = copy(ddCT)[, (genes) := 2^-.SD, .SDcols = genes]
  return(my_RQ)

}
