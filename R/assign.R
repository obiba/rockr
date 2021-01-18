#' Assign an R expression
#'
#' Assign an R expression to a symbol in the remote R session.
#'
#' @family execution functions
#' @param conn A rockr connection object.
#' @param symbol Name of the R symbol.
#' @param value Value to assign to the symbol: can be a raw value or a R script to execute.
#' @param async R script is executed asynchronously within the session (default is FALSE).
#'   If TRUE, the value returned is the ID of the command to look for.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.open(conn)
#' rockr.assign(conn, "x", 123)
#' rockr.assign(conn, "y", "abc")
#' rockr.assign(conn, "z", quote(tibble::tribble(
#'   ~colA, ~colB,
#'   'a',   1,
#'   'b',   2,
#'   'c',   3
#' )))
#' rockr.close(conn)
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
