#' Evaluate a R script
#'
#' Execute a R script in the remote R session.
#'
#' @family execution functions
#' @param conn A rockr connection object.
#' @param script R script to execute.
#' @param json Response is in JSON format or an object serialized by R. Default is FALSE.
#' @param async R script is executed asynchronously within the session (default is FALSE).
#'   If TRUE, the value returned is the command object to look for.
#' @return A unserialized R object.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.open(conn)
#' rockr.eval(conn, "x <- 'foo'")
#' rockr.eval(conn, "ls()")
#' rockr.close(conn)
#' }
#' @export
#' @import httr
rockr.eval <- function(conn, script, json=FALSE, async=FALSE) {
  body <- .deparse(script)
  query <- list(async = async)
  if (json) {
    rockr.post(conn, "r", "session", conn$session$id, "_eval", query = query, body = body, acceptType = "application/json")
  } else {
    rockr.post(conn, "r", "session", conn$session$id, "_eval", query = query, body = body)
  }
}
