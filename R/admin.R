#' Status of the R server
#'
#' Get the status of the R server, including information about the server node.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.status(conn)
#' }
#' @export
#' @import httr
rockr.status <- function(conn) {
  rockr.get(conn, "rserver")
}

#' Start the R server
#'
#' Start the R server, if not already running.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @return Logical indicating wether the R server is running (invisible).
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.start(conn)
#' }
#' @export
#' @import httr
rockr.start <- function(conn) {
  info <- rockr.put(conn, "rserver")
  invisible(info$running)
}

#' Stop the R server
#'
#' Stop the R server, if running. Any R sessions will be closed.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @return Logical indicating wether the R server is running (invisible).
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.stop(conn)
#' }
#' @export
#' @import httr
rockr.stop <- function(conn) {
  info <- rockr.delete(conn, "rserver")
  invisible(info$running)
}

#' Restart the R server
#'
#' Stop and start the R server. Any R sessions will be closed.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @return Logical indicating wether the R server is running (invisible).
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.stop(conn)
#' }
#' @export
#' @import httr
rockr.restart <- function(conn) {
  info <- rockr.get(conn, "rserver")
  if (info$running) {
    rockr.delete(conn, "rserver")
  }
  info <- rockr.put(conn, "rserver")
  invisible(info$running)
}
