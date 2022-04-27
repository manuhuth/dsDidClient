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
ds.computeMatrixCrossproduct <- function(x,
                       datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("computeMatrixCrossproductDS", x)
  result <- DSI::datashield.aggregate(datasources, cally)

}
