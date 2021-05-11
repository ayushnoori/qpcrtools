#' An S4 class to contain qPCR data.
#'
#' @slot dCT A \code{data.table} containing dCT data.
#' @slot ddCT A \code{data.table} containing ddCT data.
#' @slot RQ A \code{data.table} containing RQ data.
#' @slot genes A character vector listing genes corresponding to a subset of the columns of each \code{data.table} object.
#' @slot factors A character vector listing factors for statistical comparison corresponding to a subset of the columns of each \code{data.table} object.
#' @author Ayush Noori
#' @export
qPCR <- setClass("qPCR",
                 slots = list(dCT = "data.table",
                              ddCT = "data.table",
                              RQ = "data.table",
                              genes = "character",
                              factors = "character")
)

#' An S4 class to contain qPCR data.
#'
#' @param dCT A \code{data.table} containing dCT data.
#' @param ddCT A \code{data.table} containing ddCT data.
#' @param RQ A \code{data.table} containing RQ data.
#' @param genes A character vector listing genes corresponding to a subset of the columns of each \code{data.table} object.
#' @param factors A character vector listing factors for statistical comparison corresponding to a subset of the columns of each \code{data.table} object.
#' @author Ayush Noori
#' @export
qPCR = function(dCT = data.table(), ddCT = data.table(), RQ = data.table(), genes = colnames(dCT), factors = character()) {

  message("Creating new qPCR object.")



  qPCR = new("qPCR", dCT = dCT, ddCT = ddCT, RQ = RQ, genes = genes, factors = factors)

  return(qPCR)

}

#' Display S4 object containing qPCR data.
#'
#' @param object An S4 object of class \code{qPCR} containing qPCR data.
#' @author Ayush Noori
setMethod("show", "qPCR", function(object) {

  cat("Genes:", object@genes, "\n")
  cat("Factors:", object@factors, "\n\n")
  if(nrow(object@dCT) > 0) { cat("dCT Data:\n"); print(head(object@dCT, 4)) }
  if(nrow(object@ddCT) > 0) { cat("ddCT Data:\n"); print(head(object@ddCT, 4)) }
  if(nrow(object@RQ) > 0) { cat("Relative Quantification Data:\n"); print(head(object@RQ, 4)) }

})
