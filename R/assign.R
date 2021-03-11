#' Assign an R expression or data
#'
#' Assign an R expression or R data to a symbol in the remote R session.
#'
#' @family assignment functions
#' @param conn A rockr connection object.
#' @param symbol Name of the R symbol.
#' @param value Value to assign to the symbol: can be a data value or a R script to execute.
#' @param async R script is executed asynchronously within the session (default is FALSE).
#'   If TRUE, the value returned is the ID of the command to look for.
#' @return The command object if async is TRUE
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='user', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.open(conn)
#' rockr.assign(conn, "mtcars", mtcars)
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
  .is.opened(conn)
  if(is.language(value) || is.function(value)) {
    rockr.assign.expr(conn, symbol, value, async)
  } else {
    rockr.assign.data(conn, symbol, value, async)
  }
}

#' Assign an R expression
#'
#' Assign an R expression to a symbol in the remote R session.
#'
#' @family assignment functions
#' @param conn A rockr connection object.
#' @param symbol Name of the R symbol.
#' @param value Value to assign to the symbol: can be a primitive value or a R script to execute.
#' @param async R script is executed asynchronously within the session (default is FALSE).
#'   If TRUE, the value returned is the ID of the command to look for.
#' @return The command object if async is TRUE
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='user', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.open(conn)
#' rockr.assign.expr(conn, "x", 123)
#' rockr.assign.expr(conn, "y", "abc")
#' rockr.assign.expr(conn, "z", quote(tibble::tribble(
#'   ~colA, ~colB,
#'   'a',   1,
#'   'b',   2,
#'   'c',   3
#' )))
#' rockr.close(conn)
#' }
#' @export
#' @import httr
rockr.assign.expr <- function(conn, symbol, value, async=FALSE) {
  .is.opened(conn)
  body <- .deparse(value)
  query <- list(s = symbol, async = async)
  resp <- rockr.post(conn, "r", "session", conn$session$id, "_assign", query = query, body = body, contentType = "application/x-rscript")
  if (async) {
    resp
  }
}

#' Assign an R object
#'
#' Assign an R object to a R symbol in the remote R session.
#'
#' @family assignment functions
#' @param conn A rockr connection object.
#' @param symbol Name of the R symbol.
#' @param value The R object to assign (data.frame, vector, etc.).
#' @param async R script is executed asynchronously within the session (default is FALSE).
#'   If TRUE, the value returned is the ID of the command to look for.
#' @return The command object if async is TRUE
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='user', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.open(conn)
#' # push a data frame to the R server
#' rockr.assign.data(o, "D", mtcars)
#' # push a vector to the R server
#' rockr.assign.data(o, "C", mtcars$cyl)
#' # push a string
#' rockr.assign.data(o, "S", "Hello!")
#' }
#' @export
#' @import jsonlite
rockr.assign.data <- function(conn, symbol, value, async=FALSE) {
  .is.opened(conn)
  body <- jsonlite::base64_enc(serialize(value, NULL))
  body <- gsub("[\r\n]", "", body)
  query <- list(s = symbol, async = async)
  resp <- rockr.post(conn, "r", "session", conn$session$id, "_assign", query = query, body = body, contentType = "application/x-rdata")
  if (async) {
    resp
  }
}
