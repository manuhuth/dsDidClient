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
ds.genProp <- function(form,
                       coefficients,
                       object,
                       constant_in_matrix = FALSE,
                       newobj = "propscore",
                       datasources = NULL,
                       invlog = TRUE){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # form is a string

  # first call
  cally <- call("genPropDS", form, coefficients, object, invlog, constant_in_matrix )
  result <- DSI::datashield.assign.expr(datasources, newobj, cally)

  return("Generate Propensity scores serverside")

}
