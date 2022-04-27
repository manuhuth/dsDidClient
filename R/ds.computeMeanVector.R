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
ds.computeMeanVector<- function(x, object_for_length = NULL,
                       newobj = NULL,
                       datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # form is a string

  # first call
  cally <- call("computeMeanVectorDS", x, object_for_length)
  result <- DSI::datashield.assign.expr(datasources, newobj, cally)


}
