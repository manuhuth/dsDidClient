#' @title Subset Dataframe
#' @description The function is used to subset a dataframe based on a specified variable and value. The subsetted dataframe is assigned to a new object on the server side.
#' @param df A string with the name of the dataframe from the server side to be subsetted.
#' @param name_variable The name of the variable to subset on.
#' @param value The value to subset the variable on.
#' @param include_zero Logical. Should zero values be included in the subset?
#' @param newobj The name of the new object to assign the subsetted dataframe to.
#' @param datasources A specific Datashield data source to which the result should be assigned.

#' @export
ds.subsetDf <- function(df, name_variable, value, include_zero,
                        newobj,
                        datasources = NULL) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }


  # second call
  cally2 <- call(
    "genDfSubsetDS",
    df, name_variable, value, include_zero
  )
  DSI::datashield.assign.expr(datasources, newobj, cally2)
}
