#' Download a file
#'
#' Download a file or a folder from the Rocker R session working directory.
#'
#' @family file functions
#' @param conn A rockr connection object.
#' @param source Path to the file in the R server file system.
#' @param destination Path to the file to be written. If omitted, file with same name in the working directory will be written.
#' @param overwrite Overwrite the destination file if TRUE. Default is FALSE.
#' @param temp Logical to specify whether the root folder is the R session's home or the temporary folder. Default is FALSE.
#' @examples
#' \dontrun{
#' conn <- rockr.login(url='https://rocker-demo.obiba.org')
#' # download a file
#' rockr.file_download(conn, 'data.csv')
#' # download, create destination folder and rename file
#' rockr.file_download(o, '/DatabaseTest.sav', 'spss/test.sav')
#' # download a folder
#' rockr.file_download(o, 'output', 'output.zip')
#' rockr.logout(o)
#' }
#' @export
rockr.file_download <- function(conn, source, destination=NULL, overwrite=FALSE, temp = FALSE) {
  path <- ifelse(is.null(destination), source, destination)
  GET(.url(conn, "r", "session", conn$session$id, "_download"), query=list(path=source, temp=temp), httr::write_disk(path, overwrite=overwrite))
}

#' Upload a file
#'
#' Upload a file into the Rocker R session working directory.
#'
#' @family file functions
#' @param conn A rockr connection object.
#' @param source Path to the file in the local file system.
#' @param destination Path of the destination file in the Rocker R session. Root folder is the session's home or temporary folder (see 'temp' parameter).
#' If empty (default behavior), the destination file name will be the same as the source one.
#' @param overwrite Overwrite the destination file if TRUE. Default is FALSE.
#' @param temp Logical to specify whether the root folder is the R session's home or the temporary folder. Default is FALSE.
#' @examples
#' \dontrun{
#' conn <- rockr.login(url='https://rocker-demo.obiba.org')
#' # upload a file
#' rockr.file_upload(conn, 'data.csv')
#' # download, create destination folder and rename file
#' rockr.file_upload(o, 'DatabaseTest.sav', '/spss/test.sav')
#' # upload a folder
#' rockr.file_upload(o, 'input')
#' rockr.logout(o)
#' }
#' @export
rockr.file_upload <- function(conn, source, destination=NULL, overwrite=FALSE, temp = FALSE) {
  POST(.url(conn, "r", "session", conn$session$id, "_upload"), body=list(file=httr::upload_file(source), path=destination, overwrite=overwrite, temp=temp),
       encode = "multipart", content_type("multipart/form-data"), accept("text/html"),
       config=conn$config, handle=conn$handle, .verbose())
}
