#' @title multiplierBootstrap
#' @description The function performs a multiplier bootstrap on a given matrix on the server side and returns the results to the client side.
#' @param matrix A string with the name of the matrix on which to perform the multiplier bootstrap.
#' @param n_iterations The number of iterations for the bootstrap.
#' @param datasources A specific Datashield data source to which the result should be assigned.
#' @export
ds.multiplierBootstrap <- function(matrix, n_iterations,
                                   datasources = NULL) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("multiplierBootstrapDS", matrix, n_iterations)
  result <- DSI::datashield.aggregate(datasources, cally)
}
