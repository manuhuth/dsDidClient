#' generate prop scores server side
#'
#' @param datasources connection
#' @param df
#' @param colname
#' @param value
#' @param minimal_number
#'
#' @return
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
