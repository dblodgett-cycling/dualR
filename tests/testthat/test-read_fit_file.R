test_that("read fit", {
  f <- read_fit_file(file.path(tmp_fit_dir, "fit/fit1.fit"))
  
  expect_s3_class(f, "data.frame")

  expect_equal(c("datetime", "heart_rate", "calories", "cadence", "power", "left_right_balance", 
                 "battery_soc", "temperature"), 
               names(f))
  
  expect_s3_class(f$datetime, "POSIXct")
    
  expect_equal(nrow(f), 6668)  
})

test_that("get meta", {
  meta <- get_fit_meta(file.path(tmp_fit_dir, "fit/fit1.fit"))
  
  expect_true(all(c("manufacturer", "product", 
                    "serial_number", "time_created", 
                    "type") %in% names(meta)))
  
  meta <- get_fit_meta(file.path(tmp_fit_dir, "fit/fit2.fit"))
  
  expect_true(all(c("manufacturer", "product", 
                    "time_created", 
                    "type") %in% names(meta)))
  
  expect_equal(meta$serial_number, NULL)
  
  expect_s3_class(meta$time_created, "POSIXct")
})

test_that("get devices", {
  meta <- get_device_meta(file.path(tmp_fit_dir, "fit/zwift/wahoo_h3.fit"))
  
  expect_equal(nrow(meta), 2)
  
  meta <- get_device_meta(file.path(tmp_fit_dir, "fit/rgt/rgt_h3.fit"))
  
  expect_true(nrow(meta) == 0)
})