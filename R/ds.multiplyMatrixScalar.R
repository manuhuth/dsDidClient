#' @title Multiply Matrix and Scalar
#' @description The function is used to multiply a matrix and a scalar and assign the result to a new object on the server side.
#' @param matrix The matrix to be multiplied.  Must be a string with the name of the matrix on the server side.
#' @param scalar The scalar to be multiplied. Must be a single number.
#' @param newobj The name of the new object to assign the result of the multiplication to on the server side.
#' @param datasources A specific Datashield data source to which the result should be assigned.

#' @export
ds.multiplyMatrixScalar <- function(matrix, scalar,
                                    newobj = NULL,
                                    datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("multiplyMatrixScalarDS", matrix, scalar)
  result <- DSI::datashield.assign.expr(datasources, newobj, cally)

}
