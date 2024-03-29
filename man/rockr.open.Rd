% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sessions.R
\name{rockr.open}
\alias{rockr.open}
\title{Open an R session}
\usage{
rockr.open(conn)
}
\arguments{
\item{conn}{A rockr connection object.}
}
\value{
Void
}
\description{
Open a R session, and store the session object within the connection object.
}
\examples{
\dontrun{
conn <- rockr.connect('administrator','password', url='https://rock-demo.obiba.org')
rockr.open(conn)
rockr.close(conn)
}
}
\seealso{
Other connection functions: 
\code{\link{rockr.close}()},
\code{\link{rockr.connect}()}
}
\concept{connection functions}
