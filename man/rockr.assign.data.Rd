% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign.R
\name{rockr.assign.data}
\alias{rockr.assign.data}
\title{Assign an R object}
\usage{
rockr.assign.data(conn, symbol, value, async = FALSE)
}
\arguments{
\item{conn}{A rockr connection object.}

\item{symbol}{Name of the R symbol.}

\item{value}{The R object to assign (data.frame, vector, etc.).}

\item{async}{R script is executed asynchronously within the session (default is FALSE).
If TRUE, the value returned is the ID of the command to look for.}
}
\value{
The command object if async is TRUE
}
\description{
Assign an R object to a R symbol in the remote R session.
}
\examples{
\dontrun{
conn <- rockr.connect(username='user', password='password',
                      url='https://rock-demo.obiba.org')
rockr.open(conn)
# push a data frame to the R server
rockr.assign.data(o, "D", mtcars)
# push a vector to the R server
rockr.assign.data(o, "C", mtcars$cyl)
# push a string
rockr.assign.data(o, "S", "Hello!")
}
}
\seealso{
Other assignment functions: 
\code{\link{rockr.assign.expr}()},
\code{\link{rockr.assign}()}
}
\concept{assignment functions}
