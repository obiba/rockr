library(rockr)

options(verbose = T, error = rlang::entrace)

#
# Assign and evaluate
#

conn <- rockr.connect(username='administrator', password='password', url = "http://localhost:6312")
rockr.open(conn)
conn

# assign different type of values
rockr.assign(conn, "x", 123)
rockr.assign(conn, "y", "abc")
rockr.assign(conn, "z", quote(tibble::tribble(
  ~colA, ~colB,
  'a',   1,
  'b',   2,
  'c',   3
)))

# evaluate expressions
rockr.eval(conn, quote(x))
rockr.eval(conn, quote(y))
rockr.eval(conn, quote(z))
rockr.eval(conn, quote(z), json = TRUE)
rockr.eval(conn, quote(ls()))
rockr.eval(conn, call("ls"))

# assign asynchronously
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

rockr.close(conn)
conn

#
# Files
#

conn <- rockr.connect(username='administrator', password='password', url = "http://localhost:6312")
rockr.open(conn)
conn

# files
rockr.eval(conn, quote(list.files()))
rockr.file_upload(conn, source = "DESCRIPTION", destination = "/foo", overwrite = FALSE)
rockr.file_upload(conn, source = "DESCRIPTION", destination = "/foo", overwrite = TRUE)
rockr.file_upload(conn, source = "DESCRIPTION", destination = "/foo", overwrite = FALSE)
rockr.file_upload(conn, source = "DESCRIPTION", destination = "/", overwrite = FALSE)
rockr.file_upload(conn, source = "DESCRIPTION", overwrite = FALSE)
rockr.eval(conn, quote(list.files()))
rockr.file_upload(conn, source = "DESCRIPTION", overwrite = FALSE, temp = TRUE)
rockr.eval(conn, quote(list.files(tempdir())))
rockr.file_download(conn, "foo", destination = "pwel", overwrite = TRUE)
rockr.file_download(conn, "foo", destination = "pwel", overwrite = FALSE)
unlink("pwel")
rockr.file_download(conn, "foo", destination = "docs/pwel", overwrite = FALSE)
rockr.file_download(conn, "bar", overwrite = TRUE)
rockr.file_download(conn, "docs/bar", overwrite = TRUE)
unlink("docs", recursive = TRUE)

# invalid file
rockr.file_upload(conn, source = "DESCRIPTION", destination = "../foo", overwrite = TRUE)
rockr.file_upload(conn, source = "DESCRIPTION", destination = "../foo", overwrite = TRUE, temp = TRUE)
rockr.eval(conn, quote(list.files("../../../conf")))
rockr.file_download(conn, source = "../../../conf/Rprofile.R", destination = "parser.conf")

# mkdirs
rockr.file_upload(conn, source = "DESCRIPTION", destination = "/somedir/description", overwrite = T)
rockr.file_upload(conn, source = "DESCRIPTION", destination = "/somedir/description", overwrite = T, temp = T)
rockr.eval(conn, quote(list.files(recursive = TRUE)))
rockr.eval(conn, quote(tempdir()))
rockr.eval(conn, quote(list.files(tempdir(), recursive = TRUE)))
rockr.eval(conn, quote(unlink(file.path(tempdir(), "somedir"), recursive = TRUE)))

rockr.close(conn)

#
# JSON data transfer format and error handling
#

conn <- rockr.connect(username='administrator', password='password', url = "http://localhost:6312")
rockr.open(conn)

rockr.get(conn, "r", "sessions")

rockr.assign(conn, "xfunc", quote(function(){stop('test')}))
rockr.eval(conn, quote(ls()))
rockr.eval(conn, quote(xfunc()))
rockr.eval(conn, quote(xfunc()), json = TRUE)
rockr.eval(conn, quote(xfunc()), async = TRUE)
rockr.eval(conn, quote(xfunc()), json = TRUE, async = TRUE)
rockr.commands(conn)

rockr.eval(conn, call("list.files", "../../.."))
rockr.eval(conn, call("list.files", "../../.."), json = TRUE)
cmd1 <- rockr.eval(conn, call("list.files", "../../.."), async = TRUE)
cmd2 <- rockr.eval(conn, call("list.files", "../../.."), json = TRUE, async = TRUE)
rockr.commands(conn)
rockr.command_result(conn, cmd1$id)
rockr.command_result(conn, cmd2$id)
rockr.commands(conn)

rockr.eval(conn, call("stop", "test"))
rockr.eval(conn, call("stop", "test"), json = TRUE)

rockr.close(conn)

#
# RAppArmor integration
#

library(rockr)
options(verbose = F, error = rlang::entrace)

# administrator has full access
conn <- rockr.connect(username='administrator', password='password', url = "http://localhost:6312")
rockr.open(conn)
rockr.eval(conn, quote(.libPaths()))
rockr.eval(conn, quote(read.table("/etc/passwd")))
rockr.close(conn)

# manager cannot use R
conn <- rockr.connect(username='manager', password='password', url = "http://localhost:6312")
rockr.open(conn)
conn

# user has access restricted by apparmor (if enabled)
conn <- rockr.connect(username='user', password='password', url = "http://localhost:6312")
rockr.open(conn)
rockr.eval(conn, quote(.libPaths()))
rockr.eval(conn, quote(read.table("/etc/passwd")))
rockr.close(conn)

#
# Administration
#

conn <- rockr.connect(username='administrator', password='password', url = "http://localhost:6312")

# start/stop/status
rockr.status(conn)
rockr.stop(conn)
rockr.status(conn)
rockr.start(conn)
rockr.status(conn)
rockr.restart(conn)
rockr.status(conn)
rockr.version(conn)
rockr.log(conn, 100)

# packages management
rockr.packages(conn)
rockr.package(conn, 'rlang')
rockr.package_rm(conn, 'rlang')
rockr.package(conn, 'rlang')
rockr.package_install(conn, 'rlang')
rockr.package(conn, 'rlang')
rockr.package_install(conn, 'datashield/DSI', manager = 'github')
