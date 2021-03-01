test_that("R session", {
  check_skip()

  conn1 <- rockr.connect(username = "administrator", password = "password")
  rockr.open(conn1)
  expect_length(rockr.sessions(conn1), 1)

  # manager cannot open
  conn2 <- rockr.connect(username = "manager", password = "password")
  expect_error(rockr.open(conn2))

  # user can see only own
  conn3 <- rockr.connect(username = "user", password = "password")
  rockr.open(conn3)
  expect_length(rockr.sessions(conn3), 1)

  # administrator/manager can see all
  expect_length(rockr.sessions(conn1), 2)
  expect_length(rockr.sessions(conn2), 2)

  rockr.close(conn1)
  rockr.close(conn3)
})
