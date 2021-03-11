#' Status of the R server
#'
#' Get the status of the R server, including information about the server node.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @return The R server status object.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rocker-demo.obiba.org')
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
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.start(conn)
#' }
#' @export
#' @import httr
rockr.start <- function(conn) {
  info <- rockr.put(conn, "rserver")
  invisible(info$rServerStatus$running)
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
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.stop(conn)
#' }
#' @export
#' @import httr
rockr.stop <- function(conn) {
  info <- rockr.delete(conn, "rserver")
  invisible(info$rServerStatus$running)
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
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rocker-demo.obiba.org')
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

#' Log of the R server
#'
#' Get the tail of the R server log.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @param limit The number of lines in the tail.
#' @return A vector of character strings, one per log file line.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.log(conn, 10)
#' }
#' @export
#' @import httr
rockr.log <- function(conn, limit=100) {
  unlist(strsplit(rockr.get(conn, "rserver", "_log", query=list(limit=limit), acceptType='text/plain'), '\n'))
}

#' Version of the R server
#'
#' Get the version object of the R server.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @return A simple.list, same as R.version.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.version(conn, 10)
#' }
#' @export
#' @import httr
rockr.version <- function(conn) {
  rockr.get(conn, "rserver", "_version")
}
