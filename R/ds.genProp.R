#' @title genProp
#' @description The function generates propensity scores for a given formula, coefficients, and ds.glm object on the server side.
#' @param form A string describing the formula for generating the propensity scores
#' @param coefficients A vector of coefficients for the formula
#' @param object The ds.glm object to be used for generating the scores
#' @param constant_in_matrix A logical indicating whether to include a constant in the matrix
#' @param newobj The name of the object to which the result should be assigned on the server side.
#' @param datasources A specific Datashield data source to which the result should be assigned.
#' @param invlog A logical indicating whether to use the inverse logit transformation.
#' @export
ds.genProp <- function(form,
                       coefficients,
                       object,
                       constant_in_matrix = FALSE,
                       newobj = "propscore",
                       datasources = NULL,
                       invlog = TRUE) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # form is a string

  # first call
  cally <- call("genPropDS", form, coefficients, object, invlog, constant_in_matrix)
  DSI::datashield.assign.expr(datasources, newobj, cally)
}
