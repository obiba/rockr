% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sessions.R
\name{rockr.session}
\alias{rockr.session}
\title{Get a R session}
\usage{
rockr.session(conn, id)
}
\arguments{
\item{conn}{A rockr connection object.}

\item{id}{The R session id to look for.}
}
\value{
The R session object.
}
\description{
Get a specific R session. Users with administrator or manager role can get any R sessions. Regular users
can only get their own.
}
\examples{
\dontrun{
conn <- rockr.connect(username='administrator', password='password',
                      url='https://rock-demo.obiba.org')
rockr.session(conn, '1234')
}
}
\seealso{
Other administration functions: 
\code{\link{rockr.log}()},
\code{\link{rockr.package_install}()},
\code{\link{rockr.package_rm}()},
\code{\link{rockr.packages_datashield}()},
\code{\link{rockr.packages_rm}()},
\code{\link{rockr.packages}()},
\code{\link{rockr.package}()},
\code{\link{rockr.restart}()},
\code{\link{rockr.session_switch}()},
\code{\link{rockr.sessions}()},
\code{\link{rockr.start}()},
\code{\link{rockr.status}()},
\code{\link{rockr.stop}()},
\code{\link{rockr.version}()}
}
\concept{administration functions}
