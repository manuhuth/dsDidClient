#' @title Send Object to Server
#' @description The function is used to send an a single number to the server side.
#' @param x The number to be sent to the server.
#' @param newobj The name of the new object to assign the sent object to on the server.
#' @param datasources A specific Datashield data source to which the result should be assigned.

#' @export
ds.sendToServer <- function(x,
                                    newobj = NULL,
                                    datasources = NULL){

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # first call
  cally <- call("sendToServerDS", x)
  result <- DSI::datashield.assign.expr(datasources, newobj, cally)

}
