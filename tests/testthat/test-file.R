test_that("File upload wd", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  # upload
  expect_length(rockr.eval(conn, quote(list.files())), 0)
  rockr.file_upload(conn, source = "test-file.R")
  expect_equal(rockr.eval(conn, quote(list.files())), c("test-file.R"))

  # overwrite
  expect_error(rockr.file_upload(conn, source = "test-file.R"))
  rockr.file_upload(conn, source = "test-file.R", overwrite = TRUE)

  # rename and make folder
  rockr.file_upload(conn, source = "test-file.R", destination = "foo/bar.R")
  expect_equal(rockr.eval(conn, quote(list.files())), c("foo", "test-file.R"))
  expect_equal(rockr.eval(conn, quote(list.files("foo"))), c("bar.R"))

  # invalid destination
  expect_error(rockr.file_upload(conn, source = "test-file.R", destination = "../bar.R"))

  rockr.close(conn)
})

test_that("File upload temp", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  # upload
  expect_length(rockr.eval(conn, quote(list.files(tempdir()))), 0)
  rockr.file_upload(conn, source = "test-file.R", temp = TRUE)
  expect_equal(rockr.eval(conn, quote(list.files(tempdir()))), c("test-file.R"))

  # overwrite
  expect_error(rockr.file_upload(conn, source = "test-file.R", temp = TRUE))
  rockr.file_upload(conn, source = "test-file.R", overwrite = TRUE, temp = TRUE)

  # rename and make folder
  rockr.file_upload(conn, source = "test-file.R", destination = "foo/bar.R", temp = TRUE)
  expect_equal(rockr.eval(conn, quote(list.files(tempdir()))), c("foo", "test-file.R"))
  expect_equal(rockr.eval(conn, quote(list.files(file.path(tempdir(), "foo")))), c("bar.R"))

  # invalid destination
  expect_error(rockr.file_upload(conn, source = "test-file.R", destination = "../bar.R", temp = TRUE))

  rockr.close(conn)
})

test_that("File download wd", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  # upload
  rockr.file_upload(conn, source = "test-file.R")
  expect_equal(rockr.eval(conn, quote(list.files())), c("test-file.R"))

  # download
  rockr.file_download(conn, source = "test-file.R", destination = file.path(tempdir(), "test-file.R"))
  expect_true("test-file.R" %in% list.files(tempdir()))

  # overwrite
  expect_error(rockr.file_download(conn, source = "test-file.R", destination = file.path(tempdir(), "test-file.R")))
  rockr.file_download(conn, source = "test-file.R", destination = file.path(tempdir(), "test-file.R"), overwrite = TRUE)

  # rename and make folder
  rockr.file_download(conn, source = "test-file.R", destination = file.path(tempdir(), "foo/bar.R"))
  expect_true("bar.R" %in% list.files(file.path(tempdir(), "foo")))

  # invalid source
  expect_error(rockr.file_download(conn, source = "../../../conf/Rprofile.R"))

  rockr.close(conn)
})

test_that("File download temp", {
  check_skip()
  conn <- rockr.connect(username = "user", password = "password")
  rockr.open(conn)

  # upload
  rockr.file_upload(conn, source = "test-file.R", temp = TRUE)
  expect_equal(rockr.eval(conn, quote(list.files(tempdir()))), c("test-file.R"))

  # download
  rockr.file_download(conn, source = "test-file.R", destination = file.path(tempdir(), "test-file-tmp.R"), temp = TRUE)
  expect_true("test-file-tmp.R" %in% list.files(tempdir()))

  rockr.close(conn)
})
