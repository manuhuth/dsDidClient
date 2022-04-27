#' generate prop scores server side
#'
#' @param object table name
#' @param name saved new table name
#' @param pos distance measure
#' @param form formula used for glm
#' @param datasources connection
#'
#' @return
#' @export
ds.computeOdds <- function(name_propensities, name_C,
                       newobj = NULL,
                       datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("computeOddsDS", name_propensities, name_C)
  result <- DSI::datashield.assign.expr(datasources, newobj, cally)

  #return("Generate Propensity scores serverside")

}
