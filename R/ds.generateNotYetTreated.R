#' @title generateNotYetTreated
#' @description The function generates a dataframe of individuals who have not yet been treated, given a dataframe, a variable name, a time variable and a group variable on the server side.
#' @param df A string name of the dataframe containing the variables to be used.
#' @param name_variable The name of the variable to be used to identify treatment status.
#' @param t A time variable indicating when the treatment occurred.
#' @param g A group variable indicating to which group the individual belongs.
#' @param newobj The name of the object to which the result should be assigned on the server side.
#' @param datasources A specific Datashield data source to which the result should be assigned.
#' @export
ds.generateNotYetTreated <- function(df, name_variable, t, g,
                                     newobj,
                                     datasources = NULL) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }


  # second call
  cally2 <- call(
    "generateNotYetTreatedDS",
    df, name_variable, t, g
  )
  result <- DSI::datashield.assign.expr(datasources, newobj, cally2)

  return(result)
}
