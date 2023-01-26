#' @title computeMatrixCrossproduct
#' @description The function computes the cross product (matrix transpose times matrix) of a matrix x which is stored on the server side and returns it to the client side.
#' @param x A string of the matrix for which the cross product is to be computed.
#' @param datasources A specific Datashield data source to which the result should be assigned.
#' @export
ds.computeMatrixCrossproduct <- function(x,
                       datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("computeMatrixCrossproductDS", x)
  result <- DSI::datashield.aggregate(datasources, cally)

}
