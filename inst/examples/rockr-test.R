library(rockr)
options(verbose = T, error = rlang::entrace)
conn <- rockr.login(username='administrator', password='password', url = "http://localhost:6312")
conn
rockr.assign(conn, "x", 123)
rockr.assign(conn, "y", "abc")
rockr.assign(conn, "z", quote(tibble::tribble(
  ~colA, ~colB,
  'a',   1,
  'b',   2,
  'c',   3
)))

rockr.eval(conn, quote(x))
rockr.eval(conn, quote(y))
rockr.eval(conn, quote(z))
rockr.eval(conn, quote(ls()))
rockr.eval(conn, call("ls"))

cmd <- rockr.assign(conn, "n", 123, async = TRUE)
cmd <- rockr.assign(conn, "z", quote(tibble::tribble(
  ~colA, ~colB,
  'a',   1,
  'b',   2,
  'c',   3
)), async = TRUE)
cmd <- rockr.eval(conn, quote(z), async = TRUE)
rockr.command_result(conn, cmd$id, wait = TRUE, rm = FALSE)

cmds <- rockr.commands(conn)
cmds
rockr.command(conn, cmds$id[1], wait = TRUE)
rockr.command_result(conn, cmds$id[1], wait = TRUE)
rockr.command(conn, cmds$id[2], wait = TRUE)
rockr.command_result(conn, cmds$id[2], wait = TRUE)
rockr.command(conn, cmds$id[3], wait = TRUE)
rockr.command_result(conn, cmds$id[3], wait = TRUE)
rockr.logout(conn)

conn <- rockr.login(username='administrator', password='password', url = "http://localhost:6312")
getwd()
list.files()
rockr.eval(conn, call("list.files"))
rockr.file_upload(conn, source = "LICENSE", destination = "/somedir/license", overwrite = F)
rockr.file_upload(conn, source = "DESCRIPTION", destination = "/somedir/license", overwrite = T)
rockr.file_upload(conn, source = "DESCRIPTION", destination = "/somedir/license", overwrite = T, temp = T)
rockr.eval(conn, call("list.files", "somedir"))
list.files(rockr.eval(conn, call("getwd")))
list.files(rockr.eval(conn, call("tempdir")))

rockr.file_upload(conn, source = "LICENSE", destination = "license", overwrite = F)
rockr.eval(conn, call("list.files"))
rockr.file_download(conn, "license", overwrite = TRUE)
rockr.file_download(conn, "license", destination = "lic", overwrite = TRUE)

rockr.logout(conn)

rlang::last_trace()
