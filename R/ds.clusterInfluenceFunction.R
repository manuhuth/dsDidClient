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
ds.clusterInfluenceFunction <- function(df, influence_matrix, clustervars, idname,
                                  newobj = NULL,
                                  datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("clusterInfluenceFunctionDS", df, influence_matrix, clustervars, idname)
  result <- DSI::datashield.assign.expr(datasources, newobj, cally)

}
