test_that("scan_files works", {
  skip("Only run manually doing development")

  # Simple word match
  matches <- scan_files(files = "DESCRIPTION", "Thomas")
  expect_equal(dim(matches)[1], 1L)

  # regular expression match version numbers
  matches <- scan_files(files = "DESCRIPTION", "\\d\\.\\d\\.\\d")
  expect_equal(dim(matches)[1], 3L)

  # No matches should still return data.frame with one row with
  # no match information
  matches <- scan_files(files = "DESCRIPTION", pattern = "ab")
  expect_equal(dim(matches)[1], 1L)
})

test_that(".expand_directory works", {
  skip("Only run manually doing development")

  expect_error(.files_in_directory("man"), NA)

})

test_that("directories can be specfied", {
  skip("Only run manually doing development")

  matches <- scan_files("man", "rox")

  matches <- scan_files(c("DESCRIPTION", "man"), "rox")

})


test_that("ignore.case works", {
  skip("Only run manually doing development")

  matches <- .find_matches("DESCRIPTION", "rox")

})

test_that("file_type works", {
  skip("Only run manually doing development")
  matches <- scan_files(c("DESCRIPTION", "README.md"), "a", file_type = ".md")
})
