#' @title Multiply Matrix Matrix
#' @description The function is used to multiply two matrices and assign the result to a new object on the server side. One of the matrices can be a client side matrix.
#' @param matrix1 The first matrix to be multiplied. Can be a string or a flattened matrix
#' @param matrix2 The second matrix to be multiplied. Must be a string with the name of the matrix on the server side if the first matrix was a matrix object.
#' @param nrow1 The number of rows in the first matrix. Only needed if the first matrix is a flattened matrix.
#' @param ncol1 The number of columns in the first matrix. Only needed if the first matrix is a flattened matrix.
#' @param nrow2 The number of rows in the second matrix. Only needed if the second matrix is a flattened matrix.
#' @param ncol2 The number of columns in the second matrix. Only needed if the second matrix is a flattened matrix.
#' @param newobj The name of the new object to assign the result of the multiplication to on the server side.
#' @param datasources A specific Datashield data source to which the result should be assigned.

#' @export
ds.multiplyMatrixMatrix <- function(matrix1, matrix2, nrow1=NULL, ncol1=NULL,
                                    nrow2=NULL, ncol2=NULL,
                                    newobj = NULL,
                                    datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("multiplyMatrixMatrixDS", matrix1, matrix2, nrow1, ncol1,
                nrow2, ncol2)
  result <- DSI::datashield.assign.expr(datasources, newobj, cally)

}
