#' @title enoughIndividuals
#' @description The function checks whether there are enough individuals in a dataframe with a specified value in a specified column that is stored on the server side.
#' @param df A string name of the dataframe dataframe containing the column and the value to be checked.
#' @param colname The name of the column that should be checked.
#' @param value The value that should be checked in the specified column.
#' @param datasources A specific Datashield data source to which the result should be assigned.
#' @export
ds.enoughIndividuals <- function(df, colname, value,
                                datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("enoughIndividualsDS", df, colname, value)
  result <- DSI::datashield.aggregate(datasources, cally)

}
