test_that("Assign data sync", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  # data assign
  x <- 123
  rockr.assign(conn, "x", x)
  expect_equal(rockr.eval(conn, quote(x)), x)

  x <- "abc"
  rockr.assign(conn, "x", x)
  expect_equal(rockr.eval(conn, quote(x)), x)

  x <- tibble::tribble(
    ~colA, ~colB,
    'a',   1,
    'b',   2,
    'c',   3
  )
  rockr.assign(conn, "x", x)
  expect_equal(rockr.eval(conn, quote(x)), x)

  rockr.close(conn)
})

test_that("Assign data async", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  # data assign
  x <- 123
  cmd <- rockr.assign(conn, "x", x, async = TRUE)
  rockr.command(conn, cmd$id, wait = TRUE)
  expect_equal(rockr.eval(conn, quote(x)), x)

  x <- "abc"
  cmd <- rockr.assign(conn, "x", x, async = TRUE)
  rockr.command(conn, cmd$id, wait = TRUE)
  expect_equal(rockr.eval(conn, quote(x)), x)

  x <- tibble::tribble(
    ~colA, ~colB,
    'a',   1,
    'b',   2,
    'c',   3
  )
  cmd <- rockr.assign(conn, "x", x, async = TRUE)
  rockr.command(conn, cmd$id, wait = TRUE)
  expect_equal(rockr.eval(conn, quote(x)), x)

  rockr.close(conn)
})

test_that("Assign expr sync", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  rockr.assign(conn, "x", quote(getwd()))
  expect_false(is.null(rockr.eval(conn, quote(x))))

  rockr.close(conn)
})

test_that("Assign expr async", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  cmd <- rockr.assign(conn, "x", quote(getwd()), async = TRUE)
  rockr.command(conn, cmd$id, wait = TRUE)
  expect_false(is.null(rockr.eval(conn, quote(x))))

  rockr.close(conn)
})
