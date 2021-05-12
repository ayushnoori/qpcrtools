#' Calculate ddCT from dCT data.
#'
#' Calculate delta delta CT from delta CT.
#'
#' @param dCT Imported dCT qPCR data as a \code{data.table} (i.e., output of \code{read_dct()}).
#' @param factors A character vector listing factors for statistical comparison corresponding to a subset of the columns of each \code{data.table} object.
#' @param reference A character vector of the same length as factors, listing the level of each factor corresponding to the reference group.
#' @param metadata Columns in the dCT \code{data.table} which are not genes, but are not relevant for comparison. Either specify \code{metadata} or \code{genes}.
#' @param genes A character vector listing genes corresponding to a subset of the columns of each \code{data.table} object.
#' @return ddCT data as a \code{data.table}.
#' @author Ayush Noori
#' @export
calculate_ddct = function(dCT, factors, reference, metadata = NULL, genes = NULL) {

  # default gene assignment
  if(is.null(genes)) genes = dCT[, !c(..metadata, ..factors)] %>% colnames()

  # error checking
  if(dCT[, map(.SD, class), .SDcols = genes] %>% {. == "numeric"} %>% sum() != length(genes)) {
    stop("Not all columns in dCT specified as genes are numeric. Are you sure that you have listed all genes for analysis appropriately?") }

  # create expression to subset reference group
  create_ref = function(a, b) { paste0("get('", a, "') == '", b, "'") }
  ref = map2(factors, reference, create_ref) %>% paste(collapse = " & ")

  # calculate average of reference group
  ctrl = dCT[, map(.SD, ~mean(.x, na.rm = TRUE)), .SDcols = genes, by = factors] %>%
    .[eval(str2expression(ref)), ..genes]

  # calculate ddCT
  message("Calculating ddCT.")
  my_ddCT = copy(dCT)[, (genes) := map2(.SD, ctrl, ~.x-.y), .SDcols = genes]
  return(my_ddCT)

}
