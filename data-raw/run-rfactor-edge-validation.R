# ==============================================================================
# RUN RFACTOR ON THE SYNTHETIC RIST EDGE-CASE VALIDATION DATASET
# ==============================================================================
#
# This script evaluates Rfactor using the same synthetic 1-minute
# fixed-interval rainfall file supplied to RIST 3.99.10 during
# edge-case validation.
#
# IMPORTANT:
#
# The rainfall data are entirely synthetic and contain no observations
# from Meteo Romania or any meteorological station.
#
# The dataset tests:
#
#   1. the exact six-hour rainfall-event separation boundary;
#   2. whether positive intermediate rainfall resets the storm-break
#      clock;
#   3. rainfall amounts below, equal to, and above the 1.27-mm RIST
#      storm-break precipitation setting;
#   4. kinetic energy and EI30 for events represented by only one
#      1-minute rainfall record.
#
# Validation against RIST 3.99.10 fixed-interval input showed that:
#
#   - gaps of 6 hours or less remain within the same event;
#   - gaps greater than 6 hours start a new event;
#   - any positive rainfall observation resets the storm-break clock;
#   - the tested storm-break precipitation values did not alter event
#     grouping;
#   - single-record events receive zero kinetic energy and zero EI30.
#
# Rfactor therefore uses single_record_energy = "rist_zero" in this
# comparison.
#
# ==============================================================================


devtools::load_all()


# ------------------------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------------------------

settings <- rf_settings(
  single_record_energy = "rist_zero"
)

print(settings)


# ------------------------------------------------------------------------------
# READ THE EXACT SAME FILE THAT WILL BE GIVEN TO RIST
# ------------------------------------------------------------------------------

rist_raw <- data.table::fread(
  "tests/testthat/fixtures/RIST_edge_validation_1min_delimited.txt"
)


# ------------------------------------------------------------------------------
# REQUIRED COLUMNS
# ------------------------------------------------------------------------------

required_columns <- c(
  "An",
  "Luna",
  "Zi",
  "Ora",
  "Minut",
  "Secunda",
  "R1m"
)

missing_columns <- setdiff(
  required_columns,
  names(rist_raw)
)

if (length(missing_columns) > 0) {

  stop(
    paste0(
      "Missing required column(s): ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------------------------
# RECONSTRUCT DATETIME
# ------------------------------------------------------------------------------

datetime_text <- sprintf(
  "%04d-%02d-%02d %02d:%02d:%02d",
  rist_raw$An,
  rist_raw$Luna,
  rist_raw$Zi,
  rist_raw$Ora,
  rist_raw$Minut,
  rist_raw$Secunda
)

datetime <- as.POSIXct(
  datetime_text,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

if (anyNA(datetime)) {
  stop(
    "One or more timestamps could not be reconstructed."
  )
}


# ------------------------------------------------------------------------------
# CHECK EXACT 1-MINUTE CONTINUITY
# ------------------------------------------------------------------------------

intervals <- as.numeric(
  diff(datetime),
  units = "mins"
)

if (any(intervals != 1)) {

  bad <- which(
    intervals != 1
  )[1]

  stop(
    paste0(
      "RIST edge-validation input is not continuous at rows ",
      bad,
      " and ",
      bad + 1,
      "."
    )
  )
}


# ------------------------------------------------------------------------------
# CREATE RFACTOR RAINFALL OBJECT
# ------------------------------------------------------------------------------

rain <- data.frame(
  datetime = datetime,

  precip_mm = as.numeric(
    rist_raw$R1m
  )
)

attr(
  rain,
  "input_timezone"
) <- "UTC"

attr(
  rain,
  "expected_interval_min"
) <- 1


rf_validate_rainfall(
  rain,
  expected_interval_min = 1
)


# ------------------------------------------------------------------------------
# SHOW NON-ZERO INPUT VALUES
# ------------------------------------------------------------------------------

cat(
  "\nNON-ZERO RAINFALL RECORDS\n",
  "=========================\n\n",
  sep = ""
)

print(
  rain[
    rain$precip_mm > 0,
    ,
    drop = FALSE
  ]
)


# ------------------------------------------------------------------------------
# IDENTIFY RAINFALL EVENTS
# ------------------------------------------------------------------------------

storms <- rf_identify_storms(
  rain,
  settings = settings,
  interval_min = 1
)


# ------------------------------------------------------------------------------
# CALCULATE EVENT EROSIVITY
# ------------------------------------------------------------------------------

events <- rf_calculate_ei30(
  storms,
  settings = settings,
  interval_min = 1
)


# ------------------------------------------------------------------------------
# ADD CASE IDs
# ------------------------------------------------------------------------------

case_lookup <- data.table::fread(
  "tests/testthat/fixtures/RIST_edge_validation_cases.csv"
)

case_lookup$date <- as.Date(
  case_lookup$date
)

event_dates <- as.Date(
  events$event_start,
  tz = "UTC"
)

events$case_id <- case_lookup$case_id[
  match(
    event_dates,
    case_lookup$date
  )
]


# ------------------------------------------------------------------------------
# REORDER FOR EASY INSPECTION
# ------------------------------------------------------------------------------

display_columns <- c(
  "case_id",
  "storm_id",
  "event_start",
  "event_end",
  "duration_min",
  "precip_mm",
  "i5_mm_h",
  "i10_mm_h",
  "i15_mm_h",
  "i20_mm_h",
  "i30_mm_h",
  "i60_mm_h",
  "energy_mj_ha",
  "ei30",
  "omit_precip_condition",
  "omit_intensity_condition",
  "omitted",
  "erosive"
)

edge_summary <- events[
  ,
  display_columns,
  drop = FALSE
]


# ------------------------------------------------------------------------------
# DISPLAY
# ------------------------------------------------------------------------------

cat(
  "\nRFACTOR EDGE-CASE RESULTS\n",
  "=========================\n\n",
  sep = ""
)

print(
  edge_summary,
  digits = 10,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# SAVE RFACTOR EDGE-CASE VALIDATION RESULTS
# ------------------------------------------------------------------------------

data.table::fwrite(
  edge_summary,
  file =
    "tests/testthat/fixtures/rfactor_edge_validation_events.csv"
)
