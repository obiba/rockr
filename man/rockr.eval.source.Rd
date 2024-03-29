% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval.R
\name{rockr.eval.source}
\alias{rockr.eval.source}
\title{Evaluate a R file script}
\usage{
rockr.eval.source(conn, path, json = FALSE, async = FALSE)
}
\arguments{
\item{conn}{A rockr connection object.}

\item{path}{Path to the R file script to execute.}

\item{json}{Response is in JSON format or an object serialized by R. Default is FALSE.}

\item{async}{R script is executed asynchronously within the session (default is FALSE).
If TRUE, the value returned is the command object to look for.}
}
\value{
The unserialized result object.
}
\description{
Upload a R file script and execute it in the remote R session with source().
}
\examples{
\dontrun{
conn <- rockr.connect(username='user', password='password',
                      url='https://rock-demo.obiba.org')
rockr.open(conn)
rockr.eval.source(conn, "myscript.R")
rockr.close(conn)
}
}
\seealso{
Other evaluation functions: 
\code{\link{rockr.eval}()}
}
\concept{evaluation functions}
