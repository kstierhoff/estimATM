extract_cal_ECS <- function(filename) {
 
  # Read calibration file
  cal     <- readLines(filename)
  
  # extract calibration results
  Freq          <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '\\s+Frequency\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  BW.athwt      <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*MajorAxis3dbBeamAngle\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  BW.along      <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*MinorAxis3dbBeamAngle\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  Offset.athwt  <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*MajorAxisAngleOffset\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  Offset.along  <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*MinorAxisAngleOffset\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  Sa.corr       <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*SaCorrectionFactor\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  Gain          <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*TransducerGain\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  EBA           <- as.numeric(stringr::str_extract(unlist(
    stringr::str_extract_all(cal, pattern = '^[^#]*\\s*TwoWayBeamAngle\\s*=\\s*\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
  
  # create a tibble of calibration results
  cal.res <- tibble::tibble(Freq,
                            BW.athwt,
                            BW.along,
                            Offset.athwt,
                            Offset.along,
                            Sa.corr,
                            Gain,
                            EBA)
  
  # Define column names
  names(cal.res) <- c("Frequency",
                      "Beamwidth_athwartship",
                      "Beamwidth_alongship",
                      "OffsetAngle_athwartship",
                      "OffsetAngle_alongship",
                      "Sa_correction",
                      "Gain",
                      "TwoWayBeamAngle")
  
  # Return the cal.res variable
  return(cal.res)
}
