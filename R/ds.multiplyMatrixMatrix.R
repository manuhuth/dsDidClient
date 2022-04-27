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
