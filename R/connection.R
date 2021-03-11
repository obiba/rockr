
#' Connect with Rocker.
#'
#' @title Rocker connection
#'
#' @family connection functions
#' @return A rockr connection object.
#' @param username User name in Rocker R server. Can be provided by "rock.username" option.
#' @param password User password in Rocker R server. Can be provided by "rock.password" option.
#' @param url Rocker R server url. Can be provided by "rock.url" option.
#' @param opts Curl options as described by httr (call httr::httr_options() for details). Can be provided by "rock.opts" option.
#' @export
#' @import httr
#' @examples
#' \dontrun{
#' #### The below examples illustrate the different ways to connect to rockr ####
#'
#' # explicit username/password connection
#' conn <- rockr.connect(username='administrator', password='password',
#'                     url='https://rocker-demo.obiba.org')
#'
#' # explicit access token connection
#' conn <- rockr.connect(token='HYG16LO0VaX4O0UardNbiqmr2ByBpRke',
#'                     url='https://rocker-demo.obiba.org')
#'
#' # connect using options and user credentials
#' options(rockr.username='administrator',
#'  rockr.password='password',
#'  rockr.url='https://rocker-demo.obiba.org')
#' conn <- rockr.connect()
#'
#' # connect using options and personal access token
#' options(rockr.token='HYG16LO0VaX4O0UardNbiqmr2ByBpRke',
#'  rockr.url='https://rocker-demo.obiba.org')
#' conn <- rockr.connect()
#'
#' # connect using ssl key pair
#' options(rockr.opts=list(
#'    sslcert='my-publickey.pem',
#'    sslkey='my-privatekey.pem'))
#' conn <- rockr.connect(url='https://rocker-demo.obiba.org')
#'}
rockr.connect <- function(username=getOption("rock.username"), password=getOption("rock.password"),
                        url=getOption("rock.url"), opts=getOption("rock.opts", list())) {
  if (is.null(url)) stop("Rocker R server url is required", call.=FALSE)
  conn <- new.env(parent=globalenv())
  # Username
  conn$username <- username
  # Strip trailing slash
  conn$url <- sub("/$", "", url)
  # Domain name
  conn$name <- gsub("[:/].*", "", gsub("http[s]*://", "", conn$url))
  # Version default value
  conn$version <- NA
  # Server response encoding
  conn$encoding <- "UTF-8"
  if (!is.null(opts$encoding)) {
    conn$encoding <- opts$encoding
    opts$encoding <- NULL # not a httr/curl option
  }

  # httr/curl options
  protocol <- strsplit(url, split="://")[[1]][1]
  options <- opts
  # legacy RCurl options to httr
  if (!is.null(options$ssl.verifyhost)) {
    options$ssl_verifyhost = options$ssl.verifyhost
    options$ssl.verifyhost <- NULL
  }
  if (!is.null(options$ssl.verifypeer)) {
    options$ssl_verifypeer = options$ssl.verifypeer
    options$ssl.verifypeer <- NULL
  }

  # authentication strategies
  if(!is.na(username) && !is.null(username) && nchar(username) > 0
     && !is.na(password) && !is.null(password) && nchar(password) > 0) {
    # Authorization header
    conn$authorization <- .authorizationHeaderRockr(username, password)
  } else {
    stop("rockr authentication strategy not identified: either provide username/password or API access token or SSL certificate/private keys", call.=FALSE)
  }

  conn$config <- httr::config()
  conn$config$options <- options
  conn$handle <- httr::handle(paste0(conn$url, "/", sample(1000:9999, 1))) # append a random number to ensure urls are different
  conn$rid <- NULL
  class(conn) <- "rockr"
  conn
}

#' @export
print.rockr <- function(x, ...) {
  cat("url:", x$url, "\n")
  cat("name:", x$name, "\n")
  if (!is.null(x$session)) {
    cat("session:\n")
    cat("  id:", x$session$id, "\n")
    cat("  subject:", x$session$subject, "\n")
    cat("  createdDate:", x$session$createdDate, "\n")
  }
}

#' Generic REST resource getter.
#'
#' @family REST functions
#' @param conn A rockr connection object.
#' @param ... Resource path segments.
#' @param query Named list of query parameters.
#' @param callback A callback function to handle the response object.
#' @import httr
#' @keywords internal
rockr.get <- function(conn, ..., query=list(), acceptType='application/octet-stream, application/json', callback=NULL) {
  r <- GET(.url(conn, ...), query=query, accept(acceptType), add_headers(Authorization = conn$authorization), config=conn$config, handle = conn$handle, .verbose())
  .handleResponseOrCallback(conn, r, callback)
}

#' Generic REST resource creation.
#'
#' @family REST functions
#' @param conn A rockr connection object.
#' @param ... Resource path segments.
#' @param query Named list of query parameters.
#' @param body The body of the request.
#' @param contentType The type of the body content. Default is 'application/x-rscript'.
#' @param acceptType The type of the body content. Default is 'application/octet-stream, application/json', i.e. a serialized R object or an error message.
#' @param callback A callback function to handle the response object.
#' @import httr
#' @keywords internal
rockr.post <- function(conn, ..., query=list(), body='', contentType='application/x-rscript', acceptType='application/octet-stream, application/json', callback=NULL) {
  r <- POST(.url(conn, ...), query=query, body=body, content_type(contentType), accept(acceptType), add_headers(Authorization = conn$authorization), config=conn$config, handle = conn$handle, .verbose())
  .handleResponseOrCallback(conn, r, callback)
}

#' Generic REST resource update.
#'
#' @family REST functions
#' @param conn A rockr connection object.
#' @param ... Resource path segments.
#' @param query Named list of query parameters.
#' @param body The body of the request.
#' @param contentType The type of the body content. Default is 'application/json'.
#' @param callback A callback function to handle the response object.
#' @import httr
#' @keywords internal
rockr.put <- function(conn, ..., query=list(), body='', contentType='application/json', callback=NULL) {
  r <- PUT(.url(conn, ...), query=query, body=body, content_type(contentType), add_headers(Authorization = conn$authorization), config=conn$config, handle = conn$handle, .verbose())
  .handleResponseOrCallback(conn, r, callback)
}

#' Generic REST resource deletion.
#'
#' @family REST functions
#' @param conn A rockr connection object.
#' @param ... Resource path segments.
#' @param query Named list of query parameters.
#' @param callback A callback function to handle the response object.
#' @import httr
#' @keywords internal
rockr.delete <- function(conn, ..., query=list(), callback=NULL) {
  r <- DELETE(.url(conn, ...), query=query, add_headers(Authorization = conn$authorization), config=conn$config, handle = conn$handle, .verbose())
  .handleResponseOrCallback(conn, r, callback)
}

#' Utility method to build urls. Concatenates all arguments and adds a '/' separator between each element
#' @import utils
#' @keywords internal
.url <- function(conn, ...) {
  utils::URLencode(paste(conn$url, paste(c(...), collapse="/"), sep="/"))
}

#' Constructs the value for the Opal Authorization header
#' @import jsonlite
#' @keywords internal
.authorizationHeaderRockr <- function(username, password) {
  paste("Basic", jsonlite::base64_enc(paste(username, password, sep=":")))
}

#' Constructs the value for the Bearer Authorization header
#' @import jsonlite
#' @keywords internal
.authorizationHeaderBearer <- function(jwt) {
  paste("Bearer", jwt)
}

#' Constructs the value for the X-Opal-Auth header
#' @import jsonlite
#' @keywords internal
.tokenHeader <- function(token) {
  token
}

#' Check if token is a JWT one
#' @keywords internal
.is.JWT <- function(token) {
  strings <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(strings) == 3) {
    jwt.payload <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(strings[2])))
    if (.is.verbose()) {
      message(rawToChar(jsonlite::base64_dec(strings[2])))
    }
    TRUE
  } else {
    FALSE
  }
}

#' Process response with default handler or the provided one
#' @keywords internal
.handleResponseOrCallback <- function(conn, response, callback=NULL) {
  if (is.null(callback)) {
    .handleResponse(conn, response)
  } else {
    handler <- match.fun(callback)
    handler(conn, response)
  }
}

#' Default request response handler.
#' @keywords internal
.handleResponse <- function(conn, response) {
  #print(response)
  headers <- httr::headers(response)

  if (response$status>=300) {
    .handleError(conn, response)
  }

  disposition <- headers['Content-Disposition']
  if(!is.na(disposition) && length(grep("attachment", disposition))) {
    .handleAttachment(conn, response, as.character(disposition))
  } else {
    .handleContent(conn, response)
  }
}

#' Handle error response
#' @keywords internal
.handleError <- function(conn, response) {
  headers <- httr::headers(response)
  content <- .getContent(conn, response)
  if (.is.verbose()) {
    warning(httr::content(response, as = "text", encoding = conn$encoding), call. = FALSE)
  }
  msg <- paste0("[", http_status(response)$message, "]")
  if (!is.null(content)) {
    if (!is.null(content$message) && content$message != "") {
      stop(paste0(msg, " ", content$message), call.=FALSE)
    } else if (!is.null(content$error) && content$error != "") {
      stop(paste0(msg, " ", content$error), call.=FALSE)
    }
  }
  stop(msg, call.=FALSE)
}

#' Default request response Location handler.
#' @keywords internal
.handleResponseLocation <- function(conn, response) {
  if (response$status>=300) {
    .handleError(conn, response)
  }

  headers <- httr::headers(response)
  location <- headers[['location']]
  if(!is.na(location)) {
    substring(location, regexpr(pattern = "/ws/", location) + 3)
  } else {
    NULL
  }
}

#' @import mime
#' @keywords internal
.handleAttachment <- function(conn, response, disposition) {
  headers <- httr::headers(response)
  content <- .getContent(conn, response)

  filename <- strsplit(disposition,"\"")[[1]][2]
  filetype <- mime::guess_type(filename)
  if(is.raw(content)) {
    if (grepl("text/", headers$`content-type`) || (grepl("application/", headers$`content-type`) && grepl("text/", filetype))){
      as.character(readChar(content, length(content)))
    } else {
      readBin(content,what = raw(),length(content))
    }
  } else if (length(grep("text/", headers$`content-type`))) {
    as.character(content)
  } else {
    content
  }
}

#' @import httr
#' @keywords internal
.handleContent <- function(conn, response) {
  headers <- httr::headers(response)
  content <- .getContent(conn, response)

  if(length(grep("octet-stream", headers$`content-type`))) {
    unserialize(content)
  } else if (length(grep("text", headers$`content-type`))) {
    as.character(content)
  } else {
    content
  }
}

#' Wrapper of httr::content()
#' @import httr
#' @keywords internal
.getContent <- function(conn, response) {
  headers <- httr::headers(response)
  if (is.null(headers$`content-type`)) {
    NULL
  } else if (startsWith(headers$`content-type`, "text/")) {
    httr::content(response, as = "text", encoding = conn$encoding)
  } else {
    httr::content(response, encoding = conn$encoding)
  }
}


#' Check if response content is empty.
#' @keywords internal
.isContentEmpty <- function(content) {
  return(is.null(content)
         || (is.raw(content) && nchar(rawToChar(content))==0)
         || (is.character(content) && nchar(content)==0))
}

#' Verbose flag
#' @import httr
#' @keywords internal
.verbose <- function() {
  verbose <- NULL
  if (.is.verbose()) {
    verbose <- httr::verbose()
  }
  verbose
}

#' Verbose option
#' @keywords internal
.is.verbose <- function() {
  getOption("verbose", FALSE)
}

#' Turn expression into character strings.
#' @keywords internal
.deparse <- function(expr) {
  expression <- deparse(expr)
  if(length(expression) > 1) {
    expression = paste(expression, collapse='\n')
  }
  expression
}

#' Check connection is opened
#' @keywords internal
.is.opened <- function(conn) {
  if (is.null(conn$session)) {
    stop("Connection is not opened, use rockr.open() to create a new R session")
  }
}
