% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commands.R
\name{rockr.command_rm}
\alias{rockr.command_rm}
\title{Remove an asynchronous command}
\usage{
rockr.command_rm(conn, id)
}
\arguments{
\item{conn}{A rockr connection object.}

\item{id}{R command ID.}
}
\value{
Void
}
\description{
Remove an asynchronous R commands in the remote R session.
}
\examples{
\dontrun{
conn <- rockr.connect('administrator','password',
                      url='https://rock-demo.obiba.org')
rockr.open(conn)
rockr.command_rm(conn, '1234')
rockr.close(conn)
}
}
\seealso{
Other command functions: 
\code{\link{rockr.command_result}()},
\code{\link{rockr.commands_rm}()},
\code{\link{rockr.commands}()},
\code{\link{rockr.command}()}
}
\concept{command functions}
