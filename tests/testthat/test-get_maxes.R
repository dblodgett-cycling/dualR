test_that("examples", {
  m1 <- get_maxes(read_fit_file(system.file("fit/fit1.fit", package = "dualR")))

  m2 <- get_maxes(read_fit_file(system.file("fit/fit1.fit", package = "dualR")),
                  read_fit_file(system.file("fit/fit2.fit", package = "dualR")))

  expect_equal(m1[1], m2[1])
  
  expect_equal(m1[9], m2[9])
  
  expect_true(length(m2) < length(m1))
})
