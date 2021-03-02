test_that("wahoo", {
  devices <- get_device_meta(system.file("fit/fit1.fit", package = "dualR"))
  
  summary <- get_devices_summary(devices)  
  
  expect_equal(summary$fit$serial, "1057771008")
  
  expect_equal(summary$fit$source, "wahoo_fitness")
  
  expect_equal(summary$power$source, "saxonar")
  
})

test_that("garmin", {
  devices <- get_device_meta(system.file("fit/rgt/02_garmin_quarqd4.fit", package = "dualR"))
  
  summary <- get_devices_summary(devices)
  
  expect_equal(summary$fit$source, "garmin")
  expect_equal(summary$power$source, "tacx")
  expect_equal(summary$power$serial, "84628139")
})

test_that("garmin with saris FE", {
  # taken from a real file, faked serial numbers. 4096806322_ACTIVITY.fit

  devices <- list("1", "", "garmin", "-1234567890", "13", "local", "2019-09-05 23:02:37", NA, NA)
  names(devices) <- c("id", "device_type", "manufacturer", "serial_number", "software_version", 
                      "source_type", "timestamp", "antplus_device_type", "product")
  devices <- as.data.frame(devices)
  devices[2, ] <- c("2", NA, "garmin", "0987654321", "23", "antplus", "2019-09-05 23:02:37", "bike_cadence", NA)
  devices[3, ] <- c("3", NA, "wahoo_fitness", "87654", "1", "antplus", "2019-09-05 23:02:37", "heart_rate", "7")
  devices[4, ] <- c("4", NA, "saris", "", "59.31", "antplus", "2019-09-05 23:02:37", "fitness_equipment", "320")

  summary <- get_devices_summary(devices)
  
  expect_equal(summary$fit$source, "garmin")
  expect_equal(summary$power$source, "saris")
  expect_equal(summary$power$serial, "")
})

test_that("garmin with rotor power meter", {
  # from: 6278066811_ACTIVITY.fit

  devices <- list("1", "", "garmin", "-1234567890", "13.2", "local", "2021-02-16 02:01:12", NA, NA)
  names(devices) <- c("id", "device_type", "manufacturer", "serial_number", "software_version", 
                      "source_type", "timestamp", "antplus_device_type", "product")
  devices <- as.data.frame(devices)
  devices[2, ] <- c("2", "4", "garmin", "", "13.2", "local", "2021-02-16 02:01:12", NA, NA)
  devices[3, ] <- c("3", NA, "rotor", "87654", "10.61", "antplus", "2021-02-16 02:01:12", "bike_power", "115")
  
  summary <- get_devices_summary(devices)
  
  expect_equal(summary$fit$serial, "-1234567890")
  expect_equal(summary$power$source, "rotor")
  expect_equal(summary$power$serial, "87654")
})

test_that("rgt", {
  devices <- get_device_meta(system.file("fit/fit2.fit", package = "dualR"))
  
  expect_equal(get_devices_summary(devices), 
               list(fit = list(source = "rgt", serial = NULL, version = NULL), 
                    power = list(source = NULL, serial = NULL, version = NULL)))
})

test_that("zwift", {
  
  summary <- get_devices_summary(system.file("fit/zwift/zwift_p2m.fit", package = "dualR"))
  
  expect_equal(summary$fit$source, "zwift")
  expect_equal(summary$fit$serial, "-468985598")
  expect_equal(summary$fit$version, "5.62")
  
})