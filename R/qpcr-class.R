#' @include calculate-ddct.R
NULL

#'
#' @slot dCT A \code{data.table} containing dCT data.
#' @slot ddCT A \code{data.table} containing ddCT data.
#' @slot RQ A \code{data.table} containing RQ data.
#' @slot genes A character vector listing genes corresponding to a subset of the columns of each \code{data.table} object.
#' @slot factors A character vector listing factors for statistical comparison corresponding to a subset of the columns of each \code{data.table} object.
#' @slot reference A character vectors of length \code{factors} defining the reference group to calculate \code{ddCT}.
#' @author Ayush Noori
qPCR <- setClass("qPCR",
                 slots = list(dCT = "data.table",
                              ddCT = "data.table",
                              RQ = "data.table",
                              genes = "character",
                              factors = "character",
                              reference = "character")
)


#' An S4 class to contain qPCR data.
#'
#' The qPCR class is the main class of the \code{qpcrtools} package and is designed to contain all relevant qPCR data objects.
#'
#' @inheritParams calculate_ddct
#' @examples
#' \dontrun{qPCR(factors = c("Genotype", "Treatment"), reference = c("APOE3", "PBS"), metadata = c("Mouse", "Sex"))}
#' @author Ayush Noori
#' @export
qPCR = function(dCT, factors, reference, metadata = NULL, genes = NULL) {

  message("Creating new qPCR object.")

  # default gene assignment
  if(is.null(genes)) genes = dCT[, !c(..metadata, ..factors)] %>% colnames()

  # calculate ddCT
  my_ddCT = calculate_ddct(dCT = dCT, factors = factors, reference = reference, genes = genes)

  # calculate RQ
  my_RQ = calculate_rq(ddCT = my_ddCT, genes = genes)

  # define new qPCR object
  my_qPCR = new("qPCR", dCT = dCT, ddCT = my_ddCT, RQ = my_RQ, genes = genes, factors = factors, reference = reference)

  return(my_qPCR)

}


#' Display S4 object containing qPCR data.
#'
#' @param object An S4 object of class \code{qPCR} containing qPCR data.
#' @author Ayush Noori
setMethod("show", "qPCR", function(object) {

  cat("Genes:", object@genes, "\n")
  cat("Factors:", object@factors, "\n\n")
  if(nrow(object@dCT) > 0) { cat("dCT Data:\n"); print(head(object@dCT, 4)) }
  if(nrow(object@ddCT) > 0) { cat("\n\nddCT Data:\n"); print(head(object@ddCT, 4)) }
  if(nrow(object@RQ) > 0) { cat("\n\nRelative Quantification Data:\n"); print(head(object@RQ, 4)) }

})
