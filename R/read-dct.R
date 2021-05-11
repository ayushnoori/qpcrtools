#' @include qpcr-class.R
NULL

#' Read dCT data.
#'
#' Read dCT data from qPCR experiment.
#'
#' @param file File name of dCT data in working directory, or, alternatively, path to the qPCR data file.
#' @param format Format of the data file to import. Either \code{"csv"} to specify comma-separated data,
#' or the name or index of the sheet in the Excel workbook to read the data from.
#' @param ... Additional arguments to be passed to \code{data.table::fread()} or \code{openxlsx::read.xlsx()} as needed.
#' @return qPCR S4 object containing dCT data to be passed to next functions.
#' @author Ayush Noori
#' @export
read_dct = function(file, format = "csv", ...) {

  my_dCT = if(format == "csv") fread(file, ...) else { read.xlsx(file, format, ...) %>% as.data.table() }

  my_qPCR = qPCR(dCT = my_dCT)

  return(my_qPCR)

}
