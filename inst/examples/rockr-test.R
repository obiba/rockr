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
cmd
rockr.assign(conn, "z", quote(Stibble::tribble(
  ~colA, ~colB,
  'a',   1,
  'b',   2,
  'c',   3
)), async = TRUE)

rockr.eval(conn, "z", async = TRUE)

cmds <- rockr.commands(conn)
cmds
rockr.command(conn, cmds$id[1], wait = TRUE)
rockr.command_result(conn, cmds$id[1], wait = TRUE)
rockr.command(conn, cmds$id[2], wait = TRUE)
rockr.command_result(conn, cmds$id[2], wait = TRUE)
rockr.command(conn, cmds$id[3], wait = TRUE)
rockr.command_result(conn, cmds$id[3], wait = TRUE)

rlang::last_trace()
