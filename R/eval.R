#' Evaluate a R script
#'
#' Execute a R script in the remote R session.
#'
#' @family execution functions
#' @param conn A rockr connection object.
#' @param script R script to execute.
#' @param async R script is executed asynchronously within the session (default is FALSE).
#'   If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples
#' \dontrun{
#' conn <- rockr.login(url='https://rocker-demo.obiba.org')
#' rockr.eval(conn, "x <- 'foo'")
#' rockr.eval(o, "ls()")
#' rockr.logout(conn)
#' }
#' @export
#' @import httr
rockr.eval <- function(conn, script, async=FALSE) {
  body <- .deparse(script)
  query <- list(async = async)
  resp <- rockr.post(conn, "r", "session", conn$session$id, "_eval", query = query, body = body, contentType = "application/x-rscript")
  resp
}
