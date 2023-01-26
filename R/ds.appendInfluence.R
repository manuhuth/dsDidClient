#' @title Append Influence to a Dataframe on the server side
#' @description Appends influence to a specified column for a given set of ids and periods in a dataframe on the server side
#'
#' @param seed: Seed used for random sampling
#' @param df: The dataframe to append influence to represented as a string
#' @param influences: Vector of influences to append represented as a string
#' @param id_period_vector: Vector of ids and periods to append influence to represented as a string
#' @param column: The column in the dataframe to append the influence to represented as a string
#' @param newobj: The name of the new object
#' @param datasources: The Datashield connections to use
#' @export
ds.AppendInfluence <- function(seed, df, influences, id_period_vector, column,
                                  newobj = NULL,
                                  datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("AppendInfluenceDS", seed, df, influences, id_period_vector, column)
  result <- DSI::datashield.assign.expr(datasources, newobj, cally)

}
