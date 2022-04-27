#' match it function (pooled)
#'
#' @param object table name
#' @param name saved new table name
#' @param pos distance measure
#' @param form formula used for glm
#' @param datasources connection
#'
#' @return
#' @export
ds.subsetDf<- function(df, name_variable, value, include_zero,
                      newobj,
                      datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }


  # second call
  cally2 <- call("genDfSubsetDS",
                 df, name_variable, value, include_zero)
  result <- DSI::datashield.assign.expr(datasources, newobj, cally2)

  return(result)
}
