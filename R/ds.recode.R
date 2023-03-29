#' @title ds.recode
#' @description The function creates a new object of the given vector and replaces certain entries of this vector
#' @param x Vector in which values should be replaced
#' @param replace_with new value
#' @param replace Entry to be replaced
#' @param newobj The name of the object to which the result should be assigned.
#' @param datasources A specific Datashield data source to which the result should be assigned.
#' @export
ds.recode <- function(x, replace_with, replace,
                                  newobj = NULL,
                                  datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("recodeDS", x, replace_with, replace)
  DSI::datashield.assign.expr(datasources, newobj, cally)

}
