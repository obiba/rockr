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
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
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
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.stop(conn)
#' }
#' @export
#' @import httr
rockr.restart <- function(conn) {
  info <- rockr.get(conn, "rserver")
  if (info$rServerStatus$running) {
    rockr.delete(conn, "rserver")
  }
  info <- rockr.put(conn, "rserver")
  invisible(info$rServerStatus$running)
}

#' Log of the R server
#'
#' Get the tail of the R server log.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @param limit The number of lines in the tail.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
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
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.version(conn, 10)
#' }
#' @export
#' @import httr
rockr.version <- function(conn) {
  rockr.get(conn, "rserver", "_version")
}

#' List of the packages in the R server
#'
#' Get the list of packages from the R server.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.packages(conn)
#' }
#' @export
#' @import httr
rockr.packages <- function(conn) {
  rockr.get(conn, "rserver", "packages")
}

#' List of the settings of the DataSHIELD packages
#'
#' Discover the list of DataSHIELD packages and their settings from the R server.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.packages_datashield(conn)
#' }
#' @export
#' @import httr
rockr.packages_datashield <- function(conn) {
  rockr.get(conn, "rserver", "packages", "_datashield")
}

#' Package description from the R server
#'
#' Get the package description from the R server.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @param name The package name.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.package(conn, 'tibble')
#' }
#' @export
#' @import httr
rockr.package <- function(conn, name) {
  rockr.get(conn, "rserver", "package", name)
}

#' Install a package in the R server
#'
#' Install a package in the R server. Package can be in different kind of
#' repositories: CRAN, GitHub or Bioconductor.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @param name The package name or identifier in the source code repository.
#' @param ref The branch/commit number of the source code repositories.
#' @param manager The package manager: cran, github (gh), bioconductor (bioc).
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.package_install(conn, 'tibble')
#' }
#' @export
#' @import httr
rockr.package_install <- function(conn, name, ref=NULL, manager='cran') {
  ignore <- rockr.post(conn, "rserver", "packages", query=list(name=name, manager=manager, ref=ref))
}

#' Remove a package from the R server
#'
#' Remove a package from the R server.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @param name The package name or identifier in the source code repository.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#' rockr.package_rm(conn, 'tibble')
#' }
#' @export
#' @import httr
rockr.package_rm <- function(conn, name) {
  rockr.delete(conn, "rserver", "package", name)
}
