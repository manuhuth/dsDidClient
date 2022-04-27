#' generate prop scores server side
#'
#' @param object table name
#' @param name saved new table name
#' @param pos distance measure
#' @param form formula used for glm
#' @param datasources connection
#'
#' @return
#' @export
ds.multiplierBootstrap <- function(matrix, n_iterations,
                                  datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("multiplierBootstrapDS", matrix, n_iterations)
  result <- DSI::datashield.aggregate(datasources, cally)

}
