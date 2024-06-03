#' @title createEmptyIdMatrix
#' @description The function creates an empty matrix with unique identifiers from a dataframe and a specified number of columns on the server side.umns for the matrix. The function also allows for the option to assign the result to a new object or a specific data source.
#' @param df A string containing the name of the dataframe containing the unique identifier column.
#' @param idname The name of the column in the dataframe that serves as the unique identifier.
#' @param n_columns The number of columns for the empty matrix.
#' @param newobj The name of the object to which the result should be assigned.
#' @param datasources A specific Datashield data source to which the result should be assigned.
#' @export
ds.createEmptyIdMatrix <- function(df, idname, n_columns,
                                   newobj = NULL,
                                   datasources = NULL) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("createEmptyIdMatrixDS", df, idname, n_columns)
  DSI::datashield.assign.expr(datasources, newobj, cally)
}
