# ==============================================================================
# REAL-DATA VALIDATION
# Bucuresti-Filaret — all available 2025 data
# ==============================================================================
#
# This script validates Rfactor using precipitation observations from
# the Bucuresti-Filaret meteorological station.
#
# The source data were exported from the Climate Data Management System
# (CDMS) of Meteo Romania (National Meteorological Administration).
#
# The original Meteo Romania precipitation file is not distributed with
# the package or public repository.
#
# This script:
#
#   1. reads the original CDMS precipitation export through Rfactor;
#   2. uses every available rainfall observation;
#   3. allows temporal gaps;
#   4. identifies rainfall events from the actual rainfall timestamps;
#   5. calculates event EI30;
#   6. aggregates erosive EI30 by month and year;
#   7. writes event, monthly, and yearly validation results;
#   8. creates a dense 1-minute input file for comparison with RIST.
#
# Missing timestamps are not treated as an input error.
#
# For the RIST comparison only, absent timestamps are represented by
# 0 mm because a regular fixed-interval series is required for this
# validation workflow.
#
# All timestamps are interpreted as UTC.
#
# ==============================================================================


devtools::load_all()


# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

input_file <-
  "sample/R1m_cdms_2025_Bucuresti-Filaret.csv"

output_dir <-
  "data-raw/real-validation"

dataset_id <-
  "Bucuresti-Filaret_2025_available"

interval_min <- 1

settings <- rf_settings(
  single_record_energy = "rist_zero"
)


dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# READ THROUGH THE PUBLIC RFACTOR READER
# ==============================================================================

rain <- rf_read_rainfall(
  file = input_file,
  datetime_col = "Data Masurarii",
  precip_col = "R1m",
  tz = "UTC",
  expected_interval_min = interval_min
)


# ==============================================================================
# INPUT SUMMARY
# ==============================================================================

cat(
  "\nAVAILABLE-DATA INPUT SUMMARY\n",
  "============================\n",
  sep = ""
)


cat(
  "First observation:     ",
  format(
    min(rain$datetime),
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ),
  " UTC\n",
  sep = ""
)


cat(
  "Last observation:      ",
  format(
    max(rain$datetime),
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ),
  " UTC\n",
  sep = ""
)


cat(
  "Available observations:",
  format(
    nrow(rain),
    big.mark = ","
  ),
  "\n",
  sep = " "
)


cat(
  "Recorded rainfall:     ",
  sum(rain$precip_mm),
  " mm\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# Describe gaps for validation information only.
#
# Gaps do NOT stop the calculation.
# ------------------------------------------------------------------------------

if (nrow(rain) > 1) {

  intervals <- as.numeric(
    diff(
      rain$datetime
    ),
    units = "mins"
  )

  gaps <- intervals >
    interval_min + 1e-8

  n_gaps <- sum(
    gaps
  )

  missing_intervals <- sum(
    pmax(
      round(
        intervals /
          interval_min
      ) - 1,
      0
    )
  )

} else {

  n_gaps <- 0

  missing_intervals <- 0
}


cat(
  "Temporal gaps:         ",
  n_gaps,
  "\n",
  sep = ""
)


cat(
  "Absent 1-min rows:     ",
  missing_intervals,
  "\n",
  sep = ""
)


# ==============================================================================
# IDENTIFY STORMS
# ==============================================================================

storms <- rf_identify_storms(
  rain,
  settings = settings,
  interval_min = interval_min
)


# ==============================================================================
# CALCULATE EVENT EI30
# ==============================================================================

events <- rf_calculate_ei30(
  storms,
  settings = settings,
  interval_min = interval_min
)


# ==============================================================================
# EVENT SUMMARY
# ==============================================================================

cat(
  "\nEVENT RESULTS\n",
  "=============\n",
  sep = ""
)


cat(
  "Number of storms:      ",
  nrow(events),
  "\n",
  sep = ""
)


cat(
  "Erosive storms:        ",
  sum(
    events$erosive
  ),
  "\n",
  sep = ""
)


cat(
  "Storm rainfall:        ",
  sum(
    events$precip_mm
  ),
  " mm\n",
  sep = ""
)


cat(
  "Erosive rainfall:      ",
  sum(
    events$precip_mm[
      events$erosive
    ]
  ),
  " mm\n",
  sep = ""
)


cat(
  "All-storm energy:      ",
  sum(
    events$energy_mj_ha
  ),
  " MJ/ha\n",
  sep = ""
)


cat(
  "All-storm EI30:        ",
  sum(
    events$ei30
  ),
  "\n",
  sep = ""
)


cat(
  "Erosive EI30:          ",
  sum(
    events$ei30[
      events$erosive
    ]
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# SAVE EVENT RESULTS
# ==============================================================================

event_file <- file.path(
  output_dir,
  paste0(
    dataset_id,
    "_rfactor_events.csv"
  )
)


data.table::fwrite(
  events,
  file = event_file
)


# ==============================================================================
# MONTHLY RFACTOR
# ==============================================================================

monthly_r <- rf_calculate_rfactor(
  events,
  period = "monthly"
)


cat(
  "\nMONTHLY RFACTOR RESULTS\n",
  "=======================\n",
  sep = ""
)


print(
  monthly_r
)


monthly_file <- file.path(
  output_dir,
  paste0(
    dataset_id,
    "_rfactor_monthly.csv"
  )
)


data.table::fwrite(
  monthly_r,
  file = monthly_file
)


# ==============================================================================
# YEARLY RFACTOR
# ==============================================================================

yearly_r <- rf_calculate_rfactor(
  events,
  period = "yearly"
)


cat(
  "\nYEARLY RFACTOR RESULTS\n",
  "======================\n",
  sep = ""
)


print(
  yearly_r
)


yearly_file <- file.path(
  output_dir,
  paste0(
    dataset_id,
    "_rfactor_yearly.csv"
  )
)


data.table::fwrite(
  yearly_r,
  file = yearly_file
)


# ==============================================================================
# CHECK THAT EVENT RAINFALL WAS PRESERVED
# ==============================================================================

#
# rf_identify_storms() removes dry periods outside storms, so compare
# the storm rainfall with the positive rainfall contained in the
# original input.
#

input_positive_rainfall <- sum(
  rain$precip_mm[
    rain$precip_mm > 0
  ]
)


event_rainfall <- sum(
  events$precip_mm
)


if (
  abs(
    input_positive_rainfall -
    event_rainfall
  ) > 1e-8
) {

  stop(
    paste0(
      "Rainfall total changed during storm reconstruction. ",
      "Input positive rainfall = ",
      input_positive_rainfall,
      " mm; event rainfall = ",
      event_rainfall,
      " mm."
    )
  )
}


# ==============================================================================
# CREATE RIST VALIDATION INPUT
# ==============================================================================

#
# The native Rfactor calculation above uses the original sparse/gapped
# series directly.
#
# RIST requires a regular fixed-interval input. Therefore, for RIST
# validation only, build a complete one-minute grid between the first
# and last available timestamps and assign 0 mm to timestamps absent
# from the source file.
#
# This does NOT modify the native Rfactor input.
# ==============================================================================


full_grid <- seq(
  from = min(
    rain$datetime
  ),
  to = max(
    rain$datetime
  ),
  by = interval_min * 60
)


dense_precip <- rep(
  0,
  length(
    full_grid
  )
)


matched <- match(
  as.numeric(
    rain$datetime
  ),
  as.numeric(
    full_grid
  )
)


if (anyNA(matched)) {
  stop(
    paste0(
      "One or more source observations could not be aligned ",
      "with the RIST one-minute grid."
    )
  )
}


dense_precip[
  matched
] <- rain$precip_mm


# ------------------------------------------------------------------------------
# Verify rainfall total
# ------------------------------------------------------------------------------

if (
  abs(
    sum(dense_precip) -
    sum(rain$precip_mm)
  ) > 1e-8
) {

  stop(
    "Rainfall total changed while creating the RIST input."
  )
}


# ------------------------------------------------------------------------------
# Build seven-column RIST input
# ------------------------------------------------------------------------------

rist_delimited <- data.frame(

  An = as.integer(
    format(
      full_grid,
      "%Y",
      tz = "UTC"
    )
  ),

  Luna = as.integer(
    format(
      full_grid,
      "%m",
      tz = "UTC"
    )
  ),

  Zi = as.integer(
    format(
      full_grid,
      "%d",
      tz = "UTC"
    )
  ),

  Ora = as.integer(
    format(
      full_grid,
      "%H",
      tz = "UTC"
    )
  ),

  Minut = as.integer(
    format(
      full_grid,
      "%M",
      tz = "UTC"
    )
  ),

  Secunda = as.integer(
    format(
      full_grid,
      "%S",
      tz = "UTC"
    )
  ),

  R1m = sprintf(
    "%.3f",
    dense_precip
  )
)


rist_file <- file.path(
  output_dir,
  paste0(
    dataset_id,
    "_RIST.txt"
  )
)


data.table::fwrite(
  rist_delimited,
  file = rist_file,
  sep = ",",
  quote = FALSE
)


# ==============================================================================
# RIST EXPORT SUMMARY
# ==============================================================================

cat(
  "\nRIST VALIDATION EXPORT\n",
  "======================\n",
  sep = ""
)


cat(
  "Dense observations:     ",
  format(
    nrow(
      rist_delimited
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Rainfall total:         ",
  sum(
    dense_precip
  ),
  " mm\n",
  sep = ""
)


cat(
  "Inserted zero rows:     ",
  length(
    full_grid
  ) -
    nrow(
      rain
    ),
  "\n",
  sep = ""
)


# ==============================================================================
# FINISHED
# ==============================================================================

cat(
  "\nFILES CREATED\n",
  "=============\n",
  sep = ""
)


cat(
  "Rfactor events:\n  ",
  event_file,
  "\n\n",
  sep = ""
)


cat(
  "Rfactor monthly:\n  ",
  monthly_file,
  "\n\n",
  sep = ""
)


cat(
  "Rfactor yearly:\n  ",
  yearly_file,
  "\n\n",
  sep = ""
)


cat(
  "RIST validation input:\n  ",
  rist_file,
  "\n",
  sep = ""
)
