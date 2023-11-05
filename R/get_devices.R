# from the ANT reference code
# public static class AntplusDeviceType {
#   public const byte Antfs = 1;
#   public const byte BikePower = 11;
#   public const byte EnvironmentSensorLegacy = 12;
#   public const byte MultiSportSpeedDistance = 15;
#   public const byte Control = 16;
#   public const byte FitnessEquipment = 17;
#   public const byte BloodPressure = 18;
#   public const byte GeocacheNode = 19;
#   public const byte LightElectricVehicle = 20;
#   public const byte EnvSensor = 25;
#   public const byte Racquet = 26;
#   public const byte ControlHub = 27;
#   public const byte MuscleOxygen = 31;
#   public const byte BikeLightMain = 35;
#   public const byte BikeLightShared = 36;
#   public const byte BikeRadar = 40;
#   public const byte WeightScale = 119;
#   public const byte HeartRate = 120;
#   public const byte BikeSpeedCadence = 121;
#   public const byte BikeCadence = 122;
#   public const byte BikeSpeed = 123;
#   public const byte StrideSpeedDistance = 124;
#   public const byte Invalid = (byte)0xFF;
# }

#' get devices summary
#' @description given device table from a fit file, return a summary of connected devices.
#' @param devices data.table of devices or path to fit file where devices can be retrieved.
#' @importFrom dplyr filter distinct
#' @export
get_devices_summary <- function(devices) {
  if(is.character(devices) | inherits(devices, "FitFile")) devices <- get_device_meta(devices)
  
  out <- list(fit = list(source = NULL, serial = NULL, version = NULL), 
              power = list(source = NULL, serial = NULL, version = NULL))
  
  if(nrow(devices) == 0) {
    out$fit$source <- "unknown"
  } else if(any(grepl("wahoo", devices$manufacturer)) & "ant_device_number" %in% names(devices)) {
    head <- distinct(filter(devices, .data$manufacturer == "wahoo_fitness" & is.na(.data$ant_device_number)))
    power <- distinct(filter(devices, grepl("^power$", .data$product_name, ignore.case = TRUE)))
    
    if(nrow(head) > 0) {
      out$fit$source <- head$manufacturer
      out$fit$serial <- head$serial_number
      out$fit$version <- head$software_version
    }
    
    if(nrow(power) > 0) {
      out$power$source <- power$manufacturer
      out$power$serial <- ifelse(!is.na(power$serial_number), power$serial_number, power$ant_device_number)
      out$power$version <- power$software_version
    }
    
  } else if(any(grepl("garmin", devices$manufacturer))) {
    if("source_type" %in% names(devices)) {
      head <- distinct(filter(devices, .data$manufacturer == "garmin" & .data$source_type == "local"))
      
      if(nrow(head) > 1) {
        head <- filter(head, .data$serial_number != "")
      }
      
      power <- distinct(filter(devices, .data$antplus_device_type == "fitness_equipment" | 
                                        .data$antplus_device_type == "bike_power"))
      
    } else {
      devices$device_type <- as.numeric(devices$device_type)
      
      head <- distinct(filter(devices, .data$manufacturer == "garmin" & 
                                (.data$device_type == 1 | is.na(.data$device_type))))
      power <- distinct(filter(devices, .data$device_type == 11))
      
    }
    
    if(nrow(head) > 0) {
      out$fit$source <- head$manufacturer
      out$fit$serial <- head$serial_number
      out$fit$version <- head$software_version
    }
    
    if(nrow(power) > 0) {
      out$power$source <- power$manufacturer
      out$power$serial <- power$serial_number
      out$power$version <- power$software_version
    }
    
  } else if(any(grepl("zwift", devices$manufacturer))) {
    out$fit$source <- devices$manufacturer
    out$fit$serial <- devices$serial_number
    out$fit$version <- devices$software_version
    
  } else if(any(grepl("development", devices$manufacturer))) {
    
    head <- distinct(filter(devices, manufacturer == "development"))
    
    power <- distinct(filter(devices, .data$device_type == 11))
    
    if(nrow(head) > 0) {
      out$fit$source <- head$manufacturer
      out$fit$serial <- head$serial_number
      out$fit$version <- head$software_version
    }
    
    if(nrow(power) > 0) {
      out$power$source <- power$manufacturer
      out$power$serial <- power$serial_number
      out$power$version <- power$software_version
    }
    
  }
  
  return(out)
}