test_that("R server status", {
  check_skip()
  conn <- rockr.connect(username = "administrator", password = "password")
  status <- rockr.status(conn)
  expect_true(status$running)
  expect_equal(status$cluster, "default")
  expect_equal(status$tags, list())
})

test_that("R server start/stop", {
  check_skip()
  conn <- rockr.connect(username = "administrator", password = "password")
  expect_true(rockr.status(conn)$running)

  # stop/start
  rockr.stop(conn)
  expect_false(rockr.status(conn)$running)
  rockr.start(conn)
  expect_true(rockr.status(conn)$running)

  # start twice does not fail
  rockr.start(conn)
  expect_true(rockr.status(conn)$running)

  # restart
  rockr.restart(conn)
  expect_true(rockr.status(conn)$running)

  # sessions are cleared on restart
  rockr.open(conn)
  expect_equal(length(rockr.sessions(conn)), 1)
  rockr.restart(conn)
  expect_equal(length(rockr.sessions(conn)), 0)
})

test_that("R server status - manager/user", {
  check_skip()
  conn <- rockr.connect(username = "manager", password = "password")
  status <- rockr.status(conn)
  expect_true(status$running)

  conn <- rockr.connect(username = "user", password = "password")
  expect_error(rockr.status(conn))
})
