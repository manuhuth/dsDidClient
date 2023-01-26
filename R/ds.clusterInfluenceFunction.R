#' @title clusterInfluenceFunction
#' @description The function calculates the influence of a cluster of variables on a target variable using an influence matrix on the server side.lows for the option to assign the result to a new object or a specific data source.
#' @param df A string name of the dataframe containing the target variable and the clustervars.
#' @param influence_matrix A string name of the matrix containing the influence of each variable on the target variable.
#' @param clustervars A string name of the vector of variables that make up the cluster.
#' @param idname The name of the column in the dataframe that serves as the unique identifier.
#' @param newobj The name of the object to which the result should be assigned.
#' @param datasources A specific Datashield data source to which the result should be assigned.
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
