test_that("Eval sync", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  x <- rockr.eval(conn, quote(mtcars))
  expect_equal(x, mtcars)

  x <- rockr.eval(conn, quote(mtcars), json = TRUE)
  expect_equal(x, jsonlite::fromJSON(jsonlite::toJSON(mtcars, auto_unbox = TRUE), simplifyVector = FALSE))

  rockr.close(conn)
})

test_that("Eval async", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  cmd <- rockr.eval(conn, quote(mtcars), async = TRUE)
  x <- rockr.command_result(conn, cmd$id, wait = TRUE)
  expect_equal(x, mtcars)

  rockr.close(conn)
})

test_that("Eval quote/call", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  expect_equal(rockr.eval(conn, quote(ls())), rockr.eval(conn, call("ls")))

  rockr.close(conn)
})

test_that("Eval failure", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  expect_error(rockr.eval(conn, call("stop", "test")))
  expect_error(rockr.eval(conn, NULL))

  rockr.close(conn)
})

test_that("Eval NULL", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  expect_null(rockr.eval(conn, "NULL"))
  expect_null(rockr.eval(conn, "NULL", json = TRUE))

  rockr.close(conn)
})

