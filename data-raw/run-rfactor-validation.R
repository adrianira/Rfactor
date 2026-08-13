# ==============================================================================
# RUN RFACTOR AGAINST THE SYNTHETIC RIST VALIDATION DATASET
# ==============================================================================
#
# This script calculates rainfall erosivity with Rfactor using the same
# synthetic 1-minute fixed-interval rainfall file supplied to RIST
# 3.99.10 during package validation.
#
# IMPORTANT:
#
# The rainfall data are entirely synthetic and contain no observations
# from Meteo Romania or any meteorological station.
#
# Rfactor and RIST are intentionally run from the exact same rainfall
# input so that event separation, rainfall intensity, kinetic energy,
# EI30, and event omission can be compared directly.
#
# This validation uses:
#
#   single_record_energy = "rist_zero"
#
# because validation against RIST 3.99.10 showed that events represented
# by only one rainfall record are assigned zero kinetic energy and zero
# EI30 in the tested fixed-interval mode.
#
# All other calculation settings use the Rfactor defaults, including:
#
#   - storm-break duration: 6 hours;
#   - precipitation omission threshold: 12.70 mm;
#   - intensity omission threshold: 25.40 mm/h;
#   - omission intensity duration: 15 minutes;
#   - omission logic: "all";
#   - kinetic-energy equation: Brown and Foster (1987).
#
# RIST input structure:
#
# An,Luna,Zi,Ora,Minut,Secunda,R1m
#
# where:
#
# An      = year
# Luna    = month
# Zi      = day
# Ora     = hour (00-23)
# Minut   = minute
# Secunda = second
# R1m     = precipitation during the 1-minute interval, mm
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
# READ EXACT RIST INPUT FILE
# ------------------------------------------------------------------------------

rist_raw <- data.table::fread(
  "tests/testthat/fixtures/RIST_validation_1min_delimited.txt"
)


# ------------------------------------------------------------------------------
# CHECK REQUIRED COLUMNS
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
      "RIST validation file is missing column(s): ",
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
    "One or more RIST timestamps could not be reconstructed."
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
      "RIST input is not continuous at row ",
      bad,
      ". Interval = ",
      intervals[bad],
      " minutes: ",
      datetime[bad],
      " -> ",
      datetime[bad + 1]
    )
  )
}


# ------------------------------------------------------------------------------
# CREATE STANDARD RFACTOR RAINFALL OBJECT
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


# ------------------------------------------------------------------------------
# VALIDATE THE INPUT
# ------------------------------------------------------------------------------

rf_validate_rainfall(
  rain,
  expected_interval_min = 1
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
# CALCULATE EVENT EI30
# ------------------------------------------------------------------------------

events <- rf_calculate_ei30(
  storms,
  settings = settings,
  interval_min = 1
)


# ------------------------------------------------------------------------------
# DISPLAY EVENT RESULTS
# ------------------------------------------------------------------------------

cat(
  "\nRFACTOR EVENT RESULTS\n",
  "=====================\n\n",
  sep = ""
)

print(
  events,
  digits = 10
)


# ------------------------------------------------------------------------------
# DISPLAY VALIDATION SUMMARY
# ------------------------------------------------------------------------------

cat(
  "\nVALIDATION SUMMARY\n",
  "==================\n",
  sep = ""
)

cat(
  "Total rainfall:        ",
  sum(events$precip_mm),
  " mm\n",
  sep = ""
)

cat(
  "Number of events:      ",
  nrow(events),
  "\n",
  sep = ""
)

cat(
  "Erosive events:        ",
  sum(events$erosive),
  "\n",
  sep = ""
)

cat(
  "All-event energy:      ",
  sum(events$energy_mj_ha),
  " MJ/ha\n",
  sep = ""
)

cat(
  "All-event EI30:        ",
  sum(events$ei30),
  "\n",
  sep = ""
)

cat(
  "Erosive-event energy:  ",
  sum(
    events$energy_mj_ha[
      events$erosive
    ]
  ),
  " MJ/ha\n",
  sep = ""
)

cat(
  "Erosive-event EI30:    ",
  sum(
    events$ei30[
      events$erosive
    ]
  ),
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# SAVE RFACTOR VALIDATION RESULTS
# ------------------------------------------------------------------------------

data.table::fwrite(
  events,
  file =
    "tests/testthat/fixtures/rfactor_validation_events.csv"
)
