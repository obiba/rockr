# Rock R

[![Build Status](https://travis-ci.com/obiba/rockr.svg?branch=master)](https://travis-ci.com/obiba/rockr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rockr)](https://cran.r-project.org/package=rockr)

R implementation of the Rock R server REST API. Allows to interact with a remote R session
in a stateful way.

Usage:

```
library(rockr)
conn <- rockr.login('username', 'passwd', url = 'http://localhost:6312')

# Open an R session
rockr.open(conn)

# Assign a R expression to a R symbol
rockr.assign(conn, 'x', 123)
rockr.assign(conn, 'y', 'hello')
rockr.assign(conn, 'z', quote(tibble::tribble(
  ~colA, ~colB,
  'a',   1,
  'b',   2,
  'c',   3
)))

# Evaluate a R expression
rockr.eval(conn, quote(x))
rockr.eval(conn, quote(ls()))
rockr.eval(conn, call("ls"))

# Asynchronous assignment and evaluation
cmd <- rockr.eval(conn, quote(z), async = TRUE)
rockr.command_result(conn, cmd$id, wait = TRUE)

# File upload and download
rockr.file_upload(conn, source = "foo", destination = "/somedir/bar")
rockr.file_download(conn, source = "/somedir/bar", destination = "foo2")

rockr.close(conn)
```

