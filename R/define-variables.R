#' @include qpcr-class.R
NULL

#' Define variables for statistical analysis.
#'
#' Define variables and genes of dCT dataset in qPCR analysis
#'
#' @param dCT Imported dCT qPCR data (i.e., output of \code{read_dct()}).
#' @param format Format of the data file to import. Either \code{"csv"} to specify comma-separated data,
#' @return Releveled dCT data.
#' @author Ayush Noori
#' @export
define_variables = function(dCT, factors = NULL) {

  dCT = if(format == "csv") fread(file, ...) else { read.xlsx(file, format, ...) %>% as.data.table() }

}
