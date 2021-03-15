test_that("examples", {
  m1 <- get_maxes(read_fit_file(file.path(tmp_fit_dir, "fit/fit1.fit")))

  m2 <- get_maxes(read_fit_file(file.path(tmp_fit_dir, "fit/fit1.fit")),
                  read_fit_file(file.path(tmp_fit_dir, "fit/fit2.fit")))

  expect_equal(m1[1], m2[1])
  
  expect_equal(m1[9], m2[9])
  
  expect_true(length(m2) < length(m1))
})
