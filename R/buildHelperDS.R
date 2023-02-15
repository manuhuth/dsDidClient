#' generate prop scores server side
#' @param x just a helper
#' @param datasources connection
#' @export
ds.buildHelper <- function(x,
                       datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("buildHelperDS", x)
  result <- DSI::datashield.aggregate(datasources, cally)

}
