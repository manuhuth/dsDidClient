#' @title Compute Odds Using Propensity Scores
#' @description This function computes odds using propensity scores for a given dataset and outcome variable using DataSHIELD.
#' @param name_propensities A character string indicating the name of the dataset containing the propensity scores.
#' @param name_C A character string indicating the name of the dataset containing the outcome variable.
#' @param newobj An optional character string indicating the name of the object to store the computed odds in. Default is NULL.
#' @param datasources An optional list of DataSHIELD data source connections to use.
#'
#' @return The computed odds as a DataSHIELD object.
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
