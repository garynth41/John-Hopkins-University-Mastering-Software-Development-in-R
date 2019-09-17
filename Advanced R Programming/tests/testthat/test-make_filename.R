test_that('check filename is created properly', {
  filename_2013 <- make_filename(2013)

  expect_that(filename_2013, is_a('character'))
})
