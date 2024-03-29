#' List of the packages in the R server
#'
#' Get the list of packages from the R server.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @return The matrix of the R packages, same as the one from install.packages().
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rock-demo.obiba.org')
#' rockr.packages(conn)
#' }
#' @export
#' @import httr
rockr.packages <- function(conn) {
  rockr.get(conn, "rserver", "packages")
}

#' Remove some packages from the R server
#'
#' Remove some packages from the R server.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @param names The package names.
#' @return Void
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rock-demo.obiba.org')
#' rockr.packages_rm(conn, c('rlang', 'tibble'))
#' }
#' @export
#' @import httr
rockr.packages_rm <- function(conn, names) {
  invisible(rockr.delete(conn, "rserver", "packages", query = list(name = paste0(names, collapse = ","))))
}

#' List of the settings of the DataSHIELD packages
#'
#' Discover the list of DataSHIELD packages and their settings from the R server.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @return The list of the DataSHIELD R packages properties (AggregateMethods, AssignMethods, Options)
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rock-demo.obiba.org')
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
#' @return The R package description object.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rock-demo.obiba.org')
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
#' @return Void
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rock-demo.obiba.org')
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
#' @param name The package name.
#' @return Void
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rock-demo.obiba.org')
#' rockr.package_rm(conn, 'tibble')
#' }
#' @export
#' @import httr
rockr.package_rm <- function(conn, name) {
  invisible(rockr.delete(conn, "rserver", "package", name))
}
