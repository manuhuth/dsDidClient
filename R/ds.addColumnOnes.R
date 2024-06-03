#' @title ds.addColumnOnes
#' @description The function adds a column of ones to a dataframe on the server side and assigns it to a new object on the server side.
#' @param x A dataframe or a string representing a dataframe on the server side
#' @param columns A character string representing the column names of the dataframe
#' @param newobj A character string representing the name of the new object to be created on the server side
#' @param datasources A list of data sources to assign the new object to. If not provided, all available connections will be used.
#' @export
ds.addColumnOnes <- function(x, columns,
                             newobj = NULL,
                             datasources = NULL) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("addColumnOnesDS", x, columns)
  result <- DSI::datashield.assign.expr(datasources, newobj, cally)
}
