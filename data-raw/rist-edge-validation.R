# ==============================================================================
# CREATE RIST EDGE-CASE VALIDATION DATASET
# ==============================================================================
#
# Purpose:
#
#   1. validate the exact RIST 3.99.10 fixed-interval storm-separation
#      behaviour around the six-hour storm-break boundary;
#
#   2. test whether positive intermediate rainfall resets the
#      storm-break clock, including rainfall amounts below, equal to,
#      and above the 1.27-mm RIST precipitation setting;
#
#   3. validate how RIST handles kinetic energy and EI30 for rainfall
#      events represented by only one 1-minute rainfall record.
#
# Validation showed that, for RIST 3.99.10 fixed-interval input:
#
#   - gaps of 6 hours or less remain within the same rainfall event;
#   - gaps greater than 6 hours start a new event;
#   - any positive rainfall observation resets the storm-break clock;
#   - changing the RIST storm-break precipitation setting did not alter
#     event grouping in the tested fixed-interval cases.
#
# IMPORTANT:
# These are synthetic rainfall data used only for validation.
#
# Temporal resolution: 1 minute
# Time zone: UTC
# Rainfall: precipitation during each 1-minute interval, mm
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
    "2025-08-01 00:00:00",
    tz = "UTC"
  ),
  to = as.POSIXct(
    "2025-08-11 23:59:00",
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


# ==============================================================================
# STORM-BREAK TESTS
# ==============================================================================


# ------------------------------------------------------------------------------
# CASE A — 5 h 59 min between rainfall records
#
# 01:00    5 mm
# 06:59    5 mm
#
# Expected and validated:
# ONE rainfall event.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-01 01:00:00",
  5
)

rain <- add_rain(
  rain,
  "2025-08-01 06:59:00",
  5
)


# ------------------------------------------------------------------------------
# CASE B — exactly 6 hours
#
# 01:00    5 mm
# 07:00    5 mm
#
# This tests the exact six-hour boundary.
#
# Expected and validated:
# ONE rainfall event.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-02 01:00:00",
  5
)

rain <- add_rain(
  rain,
  "2025-08-02 07:00:00",
  5
)


# ------------------------------------------------------------------------------
# CASE C — 6 h 01 min
#
# 01:00    5 mm
# 07:01    5 mm
#
# The separation is greater than the configured six-hour storm break.
#
# Expected and validated:
# TWO rainfall events.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-03 01:00:00",
  5
)

rain <- add_rain(
  rain,
  "2025-08-03 07:01:00",
  5
)


# ------------------------------------------------------------------------------
# CASE D — tiny intermediate rainfall
#
# 01:00    5.000 mm
# 04:00    0.001 mm
# 07:01    5.000 mm
#
# The 0.001-mm positive rainfall observation resets the six-hour
# storm-break clock.
#
# Expected and validated:
# ONE rainfall event.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-04 01:00:00",
  5
)

rain <- add_rain(
  rain,
  "2025-08-04 04:00:00",
  0.001
)

rain <- add_rain(
  rain,
  "2025-08-04 07:01:00",
  5
)


# ------------------------------------------------------------------------------
# CASE E — 1.269 mm intermediate rainfall
#
# The intermediate rainfall amount is deliberately below the
# 1.27-mm RIST storm-break precipitation setting.
#
# In the validated RIST 3.99.10 fixed-interval behaviour, any positive
# rainfall observation resets the storm-break clock regardless of its
# amount.
#
# Expected and validated:
# ONE rainfall event.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-05 01:00:00",
  5
)

rain <- add_rain(
  rain,
  "2025-08-05 04:00:00",
  1.269
)

rain <- add_rain(
  rain,
  "2025-08-05 07:01:00",
  5
)


# ------------------------------------------------------------------------------
# CASE F — exactly 1.270 mm intermediate rainfall
#
# The intermediate rainfall amount is exactly equal to the
# 1.27-mm RIST storm-break precipitation setting.
#
# Expected and validated:
# ONE rainfall event.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-06 01:00:00",
  5
)

rain <- add_rain(
  rain,
  "2025-08-06 04:00:00",
  1.270
)

rain <- add_rain(
  rain,
  "2025-08-06 07:01:00",
  5
)


# ------------------------------------------------------------------------------
# CASE G — 1.271 mm intermediate rainfall
#
# The intermediate rainfall amount is deliberately above the
# 1.27-mm RIST storm-break precipitation setting.
#
# Expected and validated:
# ONE rainfall event.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-07 01:00:00",
  5
)

rain <- add_rain(
  rain,
  "2025-08-07 04:00:00",
  1.271
)

rain <- add_rain(
  rain,
  "2025-08-07 07:01:00",
  5
)


# ==============================================================================
# SINGLE-RECORD / VERY SHORT STORM TESTS
# ==============================================================================


# ------------------------------------------------------------------------------
# CASE H — 5 mm in one minute
#
# P = 5 mm
# I15 = 20 mm/h
# I30 = 10 mm/h
#
# Under the default omission settings:
#
# P < 12.70 mm     -> TRUE
# I15 < 25.40 mm/h -> TRUE
#
# Therefore the event is omitted.
#
# The separate validation question is whether RIST assigns
# Energy = 0 and EI30 = 0 to a one-record event.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-08 08:00:00",
  5
)


# ------------------------------------------------------------------------------
# CASE I — 7 mm in one minute
#
# P = 7 mm < 12.70 mm
#
# I15 =
# 7 / 15 * 60
# = 28 mm/h
#
# Therefore:
#
# precipitation omission condition = TRUE
# intensity omission condition     = FALSE
#
# With the default "all" omission logic, the event is retained.
#
# This is a critical test of one-record event energy because the event
# contributes to erosivity classification despite containing only one
# rainfall record.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-09 08:00:00",
  7
)


# ------------------------------------------------------------------------------
# CASE J — 13 mm in one minute
#
# P = 13 mm > 12.70 mm
#
# Therefore the precipitation omission condition is FALSE and the
# event is retained under the default omission logic.
#
# This provides another one-record event for comparison with Case K.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-10 08:00:00",
  13
)


# ------------------------------------------------------------------------------
# CASE K — 13 mm over TWO consecutive 1-minute records
#
# 08:00    6.5 mm
# 08:01    6.5 mm
#
# This lets us compare a one-record 13-mm event with a
# two-record 13-mm event.
# ------------------------------------------------------------------------------

rain <- add_rain(
  rain,
  "2025-08-11 08:00:00",
  c(
    6.5,
    6.5
  )
)


# ==============================================================================
# SANITY CHECKS
# ==============================================================================

expected_total_precip <- 111.811

actual_total_precip <- sum(
  rain$precip_mm
)

if (
  abs(
    actual_total_precip -
    expected_total_precip
  ) > 1e-9
) {

  stop(
    paste0(
      "Unexpected total precipitation. Expected ",
      expected_total_precip,
      " mm but obtained ",
      actual_total_precip,
      " mm."
    )
  )
}


# ------------------------------------------------------------------------------
# CHECK TEMPORAL CONTINUITY
# ------------------------------------------------------------------------------

intervals <- as.numeric(
  diff(rain$datetime),
  units = "mins"
)

if (any(intervals != 1)) {
  stop(
    "Synthetic edge-case rainfall series is not continuous."
  )
}


# ==============================================================================
# CREATE THE CANONICAL RIST INPUT FILE
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

  # %H is the 24-hour clock: 00-23
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

  # Keep exactly three decimal places in the validation file.
  R1m = sprintf(
    "%.3f",
    rain$precip_mm
  )
)


# ------------------------------------------------------------------------------
# VERIFY RIST DATETIME FIELDS
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

check_intervals <- as.numeric(
  diff(check_datetime),
  units = "mins"
)

if (any(check_intervals != 1)) {
  stop(
    "RIST edge validation input is not continuous."
  )
}


# ------------------------------------------------------------------------------
# WRITE CANONICAL INPUT
# ------------------------------------------------------------------------------

data.table::fwrite(
  rist_delimited,
  file =
    "tests/testthat/fixtures/RIST_edge_validation_1min_delimited.txt",
  sep = ",",
  quote = FALSE
)


# ==============================================================================
# CASE LOOKUP — DEVELOPMENT METADATA ONLY
# ==============================================================================

case_lookup <- data.frame(
  case_id = LETTERS[1:11],

  date = as.Date(
    sprintf(
      "2025-08-%02d",
      1:11
    )
  ),

  description = c(
    "5 h 59 min separation",
    "Exactly 6 h separation",
    "6 h 01 min separation",
    "0.001 mm intermediate rainfall",
    "1.269 mm intermediate rainfall",
    "1.270 mm intermediate rainfall",
    "1.271 mm intermediate rainfall",
    "5 mm in one minute",
    "7 mm in one minute",
    "13 mm in one minute",
    "13 mm in two consecutive minutes"
  )
)

data.table::fwrite(
  case_lookup,
  file =
    "tests/testthat/fixtures/RIST_edge_validation_cases.csv"
)


# ------------------------------------------------------------------------------
# PRINT NON-ZERO RAINFALL FOR MANUAL VERIFICATION
# ------------------------------------------------------------------------------

print(
  rain[
    rain$precip_mm > 0,
    ,
    drop = FALSE
  ]
)
