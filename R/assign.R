#' Evaluate a R script
#'
#' Execute a R script in the remote R session.
#'
#' @family execution functions
#' @param conn A rockr connection object.
#' @param symbol Name of the R symbol.
#' @param value Value to assign to the symbol: can be a raw value or a R script to execute.
#' @param async R script is executed asynchronously within the session (default is FALSE).
#'   If TRUE, the value returned is the ID of the command to look for.
#' @examples
#' \dontrun{
#' conn <- rockr.login(url='https://rocker-demo.obiba.org')
#' rockr.eval(conn, "x <- 'foo'")
#' rockr.eval(o, "ls()")
#' rockr.logout(conn)
#' }
#' @export
#' @import httr
rockr.assign <- function(conn, symbol, value, async=FALSE) {
  body <- .deparse(value)
  query <- list(s = symbol, async = async)
  resp <- rockr.post(conn, "r", "session", conn$session$id, "_assign", query = query, body = body, contentType = "application/x-rscript")
  if (async) {
    resp
  }
}
