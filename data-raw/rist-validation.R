# ==============================================================================
# CREATE SYNTHETIC 1-MINUTE VALIDATION DATASET
# ==============================================================================
#
# This dataset is used to compare Rfactor results against RIST.
#
# IMPORTANT:
# These are synthetic rainfall data, not observations.
#
# Temporal resolution: 1 minute
# Time zone: UTC
# Precipitation: interval rainfall depth in mm
#
# ==============================================================================


# ------------------------------------------------------------------------------
# OUTPUT DIRECTORY
# ------------------------------------------------------------------------------

dir.create(
  "tests/testthat/fixtures",
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# CREATE CONTINUOUS 1-MINUTE SERIES
# ------------------------------------------------------------------------------

datetime <- seq(
  from = as.POSIXct(
    "2025-07-01 00:00:00",
    tz = "UTC"
  ),
  to = as.POSIXct(
    "2025-07-11 00:00:00",
    tz = "UTC"
  ),
  by = "1 min"
)

rain <- data.frame(
  datetime = datetime,
  precip_mm = 0
)


# ------------------------------------------------------------------------------
# HELPER FUNCTION
# ------------------------------------------------------------------------------

add_rain <- function(
    data,
    start,
    amounts
) {

  start_time <- as.POSIXct(
    start,
    tz = "UTC"
  )

  rain_times <- seq(
    from = start_time,
    by = "1 min",
    length.out = length(amounts)
  )

  index <- match(
    rain_times,
    data$datetime
  )

  if (anyNA(index)) {
    stop(
      "Rainfall period lies outside the validation time series."
    )
  }

  data$precip_mm[index] <-
    data$precip_mm[index] +
    amounts

  data
}





# ------------------------------------------------------------------------------
# CASE 01
#
# 30 mm in 30 minutes
#
# Expected:
# total = 30 mm
# I15 = 60 mm/h
# I30 = 60 mm/h
# erosive = TRUE
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  start = "2025-07-01 12:00:00",
  amounts = rep(
    1,
    30
  )
)


# ------------------------------------------------------------------------------
# CASE 02
#
# 30 mm from 12:20 through 12:49
#
# This deliberately crosses the conventional 12:30 boundary.
#
# True continuous rolling I30:
# 30 mm / 0.5 h = 60 mm/h
#
# A fixed 00/30-minute calculation would not give the same result.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  start = "2025-07-02 12:20:00",
  amounts = rep(
    1,
    30
  )
)


# ------------------------------------------------------------------------------
# CASE 03
#
# 10 mm in 10 minutes
#
# For a 30-minute intensity:
#
# I30 = 10 mm / 0.5 h
#     = 20 mm/h
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  start = "2025-07-03 12:00:00",
  amounts = rep(
    1,
    10
  )
)


# ------------------------------------------------------------------------------
# CASE 04
#
# 5 mm uniformly over 30 minutes
#
# P < 12.7 mm
# AND
# I15 < 25.4 mm/h
#
# Therefore omitted under the default Rfactor omission settings.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  start = "2025-07-04 12:00:00",
  amounts = c(
    rep(0.167, 20),
    rep(0.166, 10)
  )
)


# ------------------------------------------------------------------------------
# CASE 05
#
# 7 mm in 7 minutes
#
# P = 7 mm < 12.7 mm
#
# I15 =
# 7 / 15 * 60
# = 28 mm/h
#
# Therefore:
# precipitation omission condition = TRUE
# intensity omission condition     = FALSE
# erosive                           = TRUE
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  start = "2025-07-05 12:00:00",
  amounts = rep(
    1,
    7
  )
)


# ------------------------------------------------------------------------------
# CASE 06
#
# Exactly 12.70 mm over 120 minutes.
#
# The RIST setting says:
#
# "Precipitation LESS THAN 12.70 mm"
#
# Therefore exactly 12.70 mm should NOT satisfy the omission condition.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  start = "2025-07-06 08:00:00",
  amounts = c(
    rep(0.106, 100),
    rep(0.105, 20)
  )
)


# ------------------------------------------------------------------------------
# CASE 07
#
# 6.35 mm in 15 minutes.
#
# I15 =
# 6.35 / 15 * 60
# = 25.40 mm/h
#
# The RIST-compatible criterion is intensity strictly less than
# 25.40 mm/h, so exactly 25.40 mm/h does not satisfy the intensity
# omission condition.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  start = "2025-07-07 12:00:00",
  amounts = c(
    rep(0.423, 14),
    0.428
  )
)


# ------------------------------------------------------------------------------
# CASE 08
#
# Two rainfall pulses separated by more than six dry hours.
#
# Expected:
# TWO storms.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  start = "2025-07-08 01:00:00",
  amounts = 5
)

rain <- add_rain(
  rain,
  start = "2025-07-08 07:01:00",
  amounts = 5
)


# ------------------------------------------------------------------------------
# CASE 09
#
# 5 mm at 01:00
# 1.26 mm at 04:00
# 5 mm at 07:01
#
# Consecutive positive-rainfall observations are separated by:
#
# 01:00 -> 04:00 = 3 hours
# 04:00 -> 07:01 = 3 hours 1 minute
#
# Both gaps are shorter than the six-hour storm-break duration.
#
# Dedicated validation against RIST 3.99.10 fixed-interval input showed
# that any positive rainfall observation resets the storm-break clock,
# regardless of its rainfall amount.
#
# Expected:
# ONE rainfall event.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  start = "2025-07-09 01:00:00",
  amounts = 5
)

rain <- add_rain(
  rain,
  start = "2025-07-09 04:00:00",
  amounts = 1.26
)

rain <- add_rain(
  rain,
  start = "2025-07-09 07:01:00",
  amounts = 5
)


# ------------------------------------------------------------------------------
# CASE 10
#
# 5 mm at 01:00
# 1.27 mm at 04:00
# 5 mm at 07:01
#
# As in Case 09, each gap between consecutive positive-rainfall
# observations is shorter than six hours.
#
# The 1.27 mm intermediate rainfall observation resets the storm-break
# clock. The RIST storm-break precipitation setting is retained by
# Rfactor as configuration metadata and does not alter grouping in the
# validated fixed-interval algorithm.
#
# Expected:
# ONE rainfall event.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  start = "2025-07-10 01:00:00",
  amounts = 5
)

rain <- add_rain(
  rain,
  start = "2025-07-10 04:00:00",
  amounts = 1.27
)

rain <- add_rain(
  rain,
  start = "2025-07-10 07:01:00",
  amounts = 5
)





validation_cases <- data.frame(
  case_id = sprintf(
    "case_%02d",
    1:10
  ),

  description = c(
    "30-minute uniform rainfall event",
    "30-minute event crossing clock half-hour",
    "10-minute rainfall event",
    "Small low-intensity event",
    "Small high-intensity event",
    "Exactly 12.70 mm",
    "Exactly I15 = 25.40 mm/h",
    "Rainfall separated by more than six hours",
    "Positive 1.26 mm observation resets storm-break clock",
    "Positive 1.27 mm observation resets storm-break clock"
  ),

  date = as.Date(
    sprintf(
      "2025-07-%02d",
      1:10
    )
  )
)





# ==============================================================================
# WRITE DELIMITED INPUT FOR RIST
# ==============================================================================

rist_delimited <- data.frame(
  An = as.integer(
    format(
      rain$datetime,
      "%Y",
      tz = "UTC"
    )
  ),

  Luna = as.integer(
    format(
      rain$datetime,
      "%m",
      tz = "UTC"
    )
  ),

  Zi = as.integer(
    format(
      rain$datetime,
      "%d",
      tz = "UTC"
    )
  ),

  # IMPORTANT:
  # %H = 24-hour clock (00-23)
  # Do NOT use %I (01-12)
  Ora = as.integer(
    format(
      rain$datetime,
      "%H",
      tz = "UTC"
    )
  ),

  Minut = as.integer(
    format(
      rain$datetime,
      "%M",
      tz = "UTC"
    )
  ),

  Secunda = as.integer(
    format(
      rain$datetime,
      "%S",
      tz = "UTC"
    )
  ),

  R1m = round(
    rain$precip_mm,
    3
  )
)


# ------------------------------------------------------------------------------
# VERIFY 24-HOUR TIME
# ------------------------------------------------------------------------------

if (any(
  !rist_delimited$Ora %in% 0:23
)) {
  stop(
    "Hour values must be between 0 and 23."
  )
}


# ------------------------------------------------------------------------------
# VERIFY THAT DATETIME CAN BE PERFECTLY RECONSTRUCTED
# ------------------------------------------------------------------------------

check_datetime <- as.POSIXct(
  sprintf(
    "%04d-%02d-%02d %02d:%02d:%02d",
    rist_delimited$An,
    rist_delimited$Luna,
    rist_delimited$Zi,
    rist_delimited$Ora,
    rist_delimited$Minut,
    rist_delimited$Secunda
  ),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)


if (anyNA(check_datetime)) {
  stop(
    "Failed to reconstruct datetime from RIST fields."
  )
}


intervals <- as.numeric(
  diff(check_datetime),
  units = "mins"
)


if (any(intervals != 1)) {

  bad <- which(
    intervals != 1
  )[1]

  stop(
    paste0(
      "RIST input is not continuous. ",
      "First problem between rows ",
      bad,
      " and ",
      bad + 1,
      "."
    )
  )
}


# ------------------------------------------------------------------------------
# WRITE CANONICAL RIST / RFACTOR VALIDATION FILE
# ------------------------------------------------------------------------------

data.table::fwrite(
  rist_delimited,
  file = "tests/testthat/fixtures/RIST_validation_1min_delimited.txt",
  sep = ",",
  quote = FALSE
)


# ------------------------------------------------------------------------------
# WRITE CASE METADATA
# ------------------------------------------------------------------------------

data.table::fwrite(
  validation_cases,
  file = "tests/testthat/fixtures/rist_validation_cases.csv"
)
