test_that("read fit", {
  f <- read_fit_file(system.file("fit/fit1.fit", package = "dualR"))
  
  expect_equal(nrow(f), 6668)  
})

test_that("get meta", {
  meta <- get_fit_meta(system.file("fit/fit1.fit", package = "dualR"))
  
  expect_equal(names(meta), c("manufacturer", "product", 
                              "serial_number", "time_created", 
                              "type"))
  
  meta <- get_fit_meta(system.file("fit/fit2.fit", package = "dualR"))
  
  expect_equal(names(meta), c("manufacturer", "product", 
                              "serial_number", "time_created", 
                              "type"))
  
  expect_equal(meta$serial_number, NULL)
  
  expect_s3_class(meta$time_created, "POSIXct")
})

test_that("get devices", {
  meta <- get_device_meta(system.file("fit/zwift/wahoo_h3.fit", package = "dualR"))
  
  expect_equal(nrow(meta), 5)
  
  meta <- get_device_meta("inst/fit/rgt/rgt_h3.fit")
  
  expect_true(nrow(meta) == 0)
})