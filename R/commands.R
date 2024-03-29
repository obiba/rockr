
#' List the asynchronous commands
#'
#' Get the list of asynchronous R commands in the remote R session.
#'
#' @family command functions
#' @param conn A rockr connection object.
#' @param df Return a data.frame (default is TRUE)
#' @return The data.frame of command objects, one column per property.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='user', password='password',
#'                       url='https://rock-demo.obiba.org')
#' rockr.open(conn)
#' rockr.commands(conn)
#' rockr.close(conn)
#' }
#' @export
rockr.commands <- function(conn, df=TRUE) {
  .is.opened(conn)
  res <- rockr.get(conn, "r", "session", conn$session$id, "commands")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    id <- rep(NA, n)
    sessionId <- rep(NA, n)
    script <- rep(NA, n)
    status <- rep(NA, n)
    withResult <- rep(NA, n)
    withError <- rep(NA, n)
    error <- rep(NA, n)
    finished <- rep(NA, n)
    createdDate <- rep(NA, n)
    startDate <- rep(NA, n)
    endDate <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      id[i] <- item$id
      sessionId[i] <- item$sessionId
      script[i] <- item$script
      status[i] <- item$status
      withResult[i] <- item$withResult
      createdDate[i] <- item$createdDate
      if (!is.null(item$startDate)) {
        startDate[i] <- item$startDate
      }
      if (!is.null(item$endDate)) {
        endDate[i] <- item$endDate
      }
      if (!is.null(item$error)) {
        error[i] <- item$error
      }
      withError[i] <- item$withError
      finished[i] <- item$finished
    }
    data.frame(id, sessionId, script, status, withResult, withError, error, finished, createdDate, startDate, endDate)
  } else {
    data.frame()
  }
}

#' Get an asynchronous command
#'
#' Get an asynchronous R commands in the remote R session.
#'
#' @family command functions
#' @param conn A rockr connection object.
#' @param id R command ID.
#' @param wait Wait for the command to complete.
#' @return The command object.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='user', password='password',
#'                       url='https://rock-demo.obiba.org')
#' rockr.open(conn)
#' rockr.command(conn, '1234')
#' rockr.close(conn)
#' }
#' @export
rockr.command <- function(conn, id, wait=FALSE) {
  .is.opened(conn)
  query <- list()
  if (wait) {
    query["wait"] <- "true"
  }
  rockr.get(conn, "r", "session", conn$session$id, "command", id, query=query)
}

#' Remove an asynchronous command
#'
#' Remove an asynchronous R commands in the remote R session.
#'
#' @family command functions
#' @param conn A rockr connection object.
#' @param id R command ID.
#' @return Void
#' @examples
#' \dontrun{
#' conn <- rockr.connect('administrator','password',
#'                       url='https://rock-demo.obiba.org')
#' rockr.open(conn)
#' rockr.command_rm(conn, '1234')
#' rockr.close(conn)
#' }
#' @export
rockr.command_rm <- function(conn, id) {
  .is.opened(conn)
  ignore <- tryCatch(rockr.delete(conn, "r", "session", conn$session$id, "command", id), error=function(e){})
}

#' Remove all asynchronous commands
#'
#' Remove all asynchronous R commands in the remote R session.
#'
#' @family command functions
#' @param conn A rockr connection object.
#' @return Void
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='user', password='password',
#'                       url='https://rock-demo.obiba.org')
#' rockr.open(conn)
#' rockr.commands_rm(conn)
#' rockr.close(conn)
#' }
#' @export
rockr.commands_rm <- function(conn) {
  .is.opened(conn)
  res <- lapply(rockr.commands(conn), function(cmd) {
    rockr.command_rm(conn, cmd$id)
  })
}

#' Get result of an asynchronous command
#'
#' Get the result of an asynchronous R commands in the remote R session. The command is removed from the
#' remote R session after this call.
#'
#' @family command functions
#' @param conn A rockr connection object.
#' @param id R command ID.
#' @param wait Wait for the command to complete (default is FALSE).
#' @param rm Remove command from the list of asynchronous commands after retrieving the result (default is TRUE).
#' @return The command result as an unserialized object.
#' @examples
#' \dontrun{
#' conn <- rockr.connect(username='user', password='password',
#'                       url='https://rock-demo.obiba.org')
#' rockr.open(conn)
#' rockr.command_result(conn, '1234')
#' rockr.close(conn)
#' }
#' @export
rockr.command_result <- function(conn, id, wait = FALSE, rm = TRUE) {
  .is.opened(conn)
  if (wait) {
    cmd <- rockr.command(conn, id, wait=TRUE)
    if (cmd$status == "FAILED") {
      msg <- cmd$error
      if (is.null(cmd$error)) {
        msg <- "<no message>"
      }
      stop("Command '", cmd$script, "' failed on '", conn$url,"': ", msg, call.=FALSE)
    }
  }
  rockr.get(conn, "r", "session", conn$session$id, "command", id, "result", query = list(rm = rm))
}
