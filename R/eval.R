#' Evaluate a R script
#'
#' Execute a R script in the remote R session.
#'
#' @family evaluation functions
#' @param conn A rockr connection object.
#' @param script R script to execute.
#' @param json Response is in JSON format or an object serialized by R. Default is FALSE.
#' @param async R script is executed asynchronously within the session (default is FALSE).
#'   If TRUE, the value returned is the command object to look for.
#' @return A unserialized R object.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='user', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.open(conn)
#' rockr.eval(conn, "x <- 'foo'")
#' rockr.eval(conn, "ls()")
#' rockr.close(conn)
#' }
#' @export
#' @import httr
rockr.eval <- function(conn, script, json=FALSE, async=FALSE) {
  .is.opened(conn)
  body <- script
  if (is.language(script)) {
    body <- .deparse(script)
  }
  query <- list(async = async)
  if (json) {
    rockr.post(conn, "r", "session", conn$session$id, "_eval", query = query, body = body, acceptType = "application/json")
  } else {
    rockr.post(conn, "r", "session", conn$session$id, "_eval", query = query, body = body)
  }
}

#' Evaluate a R file script
#'
#' Upload a R file script and execute it in the remote R session with source().
#'
#' @family evaluation functions
#' @param conn A rockr connection object.
#' @param path Path to the R file script to execute.
#' @param json Response is in JSON format or an object serialized by R. Default is FALSE.
#' @param async R script is executed asynchronously within the session (default is FALSE).
#'   If TRUE, the value returned is the command object to look for.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='user', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.open(conn)
#' rockr.eval.source(conn, "myscript.R")
#' rockr.close(conn)
#' }
#' @export
rockr.eval.source <- function(conn, path, json=FALSE, async=FALSE) {
  .is.opened(conn)
  if (!file.exists(path)) {
    stop("No file at path: ", path)
  }
  if (file.info(path)$isdir) {
    stop("Cannot source a directory: ", path)
  }
  filename <- basename(path)
  rockr.file_upload(conn, path, destination = filename, overwrite = TRUE, temp = TRUE)
  script <- paste0("source(file.path(tempdir(),'", filename, "'))")
  rockr.eval(conn, script, json, async)
}
