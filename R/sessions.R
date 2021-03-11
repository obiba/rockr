#' List of the R sessions
#'
#' Get the list of the R sessions. Users with administrator or manager role can list the R sessions. Regular users
#' can only list their own.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @param subject Filter by subject name owning the sessions (requires administrator or manager role to be effective).
#' @return The list of the R session objects.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.sessions(conn)
#' }
#' @export
#' @import httr
rockr.sessions <- function(conn, subject = NULL) {
  rockr.get(conn, "r", "sessions", query = list(subject = subject))
}

#' Get a R session
#'
#' Get a specific R session. Users with administrator or manager role can get any R sessions. Regular users
#' can only get their own.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @param id The R session id to look for.
#' @return The R session object.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' rockr.session(conn, '1234')
#' }
#' @export
#' @import httr
rockr.session <- function(conn, id) {
  rockr.get(conn, "r", "session", id)
}

#' Switch R session
#'
#' Change the R session that is attached to the connection object. This allows to interact with a R session
#' after another. Of course proper permission apply when executing R operations on this R session.
#'
#' @family administration functions
#' @param conn A rockr connection object.
#' @param session A R session object.
#' @return Void
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='administrator', password='password',
#'                       url='https://rocker-demo.obiba.org')
#' # open several R sessions
#' rockr.open(conn)
#' rockr.open(conn)
#' sessions <- rockr.sessions(conn)
#' # switch to first one
#' rockr.session_switch(conn, sessions[[1]])
#' conn$session
#' rockr.close(conn)
#' # switch to 2nd one
#' rockr.session_switch(conn, sessions[[2]])
#' conn$session
#' rockr.close(conn)
#' }
#' @export
#' @import httr
rockr.session_switch <- function(conn, session) {
  conn$session <- session
}

#' Open a R session, and store the session object within the connection object.
#'
#' @title Open an R session
#'
#' @family connection functions
#' @param conn A rockr connection object.
#' @return Void
#' @examples
#' \dontrun{
#' conn <- rockr.connect('administrator','password', url='https://rocker-demo.obiba.org')
#' rockr.open(conn)
#' rockr.close(conn)
#' }
#' @export
rockr.open <- function(conn) {
  if (!is.null(conn$session)) {
    warning("Opening a new R session. Current one is not closed. See rockr.sessions() to list them and use rockr.session_switch() to change current session.")
  }
  # create an R session
  resp <- httr::POST(.url(conn, "r", "sessions"), config = conn$config, httr::add_headers(Authorization = conn$authorization, 'X-Rocker-Auth' = conn$token),
                     handle = conn$handle, content_type("application/json"), .verbose())
  if (resp$status>=300) {
    .handleError(conn, resp)
  }
  session <- content(resp)
  conn$session <- session
}

#' Close the R session, if there is any associated to the connection.
#'
#' @title Close the R session
#'
#' @family connection functions
#' @param conn A rockr connection object.
#' @return Void
#' @examples
#' \dontrun{
#' conn <- rockr.connect('administrator','password', url='https://rocker-demo.obiba.org')
#' rockr.open(conn)
#' rockr.close(conn)
#' }
#' @export
rockr.close <- function(conn) {
  if (!is.null(conn$session)) {
    ignore <- rockr.delete(conn, "r", "session", conn$session$id)
    conn$session <- NULL
  }
}

