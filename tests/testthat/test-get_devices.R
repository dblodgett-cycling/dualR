test_that("wahoo", {
  devices <- get_device_meta(file.path(tmp_fit_dir, "fit/fit1.fit"))
  
  summary <- get_devices_summary(devices)  
  
  expect_equal(summary$fit$serial, 1057771008)
  
  expect_equal(summary$fit$source, "wahoo_fitness")
  
  expect_equal(summary$power$source, "saxonar")
  
})

test_that("garmin", {
  devices <- get_device_meta(file.path(tmp_fit_dir, "fit/rgt/02_garmin_quarqd4.fit"))
  
  summary <- get_devices_summary(devices)
  
  expect_equal(summary$fit$source, "garmin")
  expect_equal(summary$power$source, "tacx")
  expect_equal(summary$power$serial, 84628139)
})

test_that("garmin with saris FE", {
  
  f <- system.file("fit/samples/4096806322_ACTIVITY.fit", package = "dualR")

  devices <- get_device_meta(f)
  
  summary <- get_devices_summary(devices)
  
  expect_equal(summary$fit$source, "garmin")
  expect_equal(summary$power$source, "saris")
  expect_equal(summary$power$serial, 0)
})

test_that("garmin with rotor power meter", {

  f <- system.file("fit/samples/6278066811_ACTIVITY.fit", package = "dualR")
  
  devices <- get_device_meta(f)
  
  summary <- get_devices_summary(devices)
  
  expect_equal(summary$fit$serial, 3921747497)
  expect_equal(summary$power$source, "rotor")
  expect_equal(summary$power$serial, 44037)
})

test_that("rgt", {
  devices <- get_device_meta(file.path(tmp_fit_dir, "fit/fit2.fit"))
  
  expect_equal(get_devices_summary(devices), 
               list(fit = list(source = "unknown", serial = NULL, version = NULL), 
                    power = list(source = NULL, serial = NULL, version = NULL)))
})

test_that("zwift", {
  
  summary <- get_devices_summary(file.path(tmp_fit_dir, "fit/zwift/zwift_p2m.fit"))
  
  expect_equal(summary$fit$source, "zwift")
  expect_equal(summary$fit$serial, 	3825981698)
  expect_equal(summary$fit$version, 5.62)
  
})