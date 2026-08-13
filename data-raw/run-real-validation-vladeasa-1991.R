# ==============================================================================
# REAL-DATA VALIDATION
# Vladeasa 1800 — available historical pluviograph data, 1991
# ==============================================================================
#
# This script validates Rfactor using historical precipitation
# observations from the Vladeasa 1800 meteorological station.
#
# The precipitation data are from Meteo Romania
# (National Meteorological Administration).
#
# The original Meteo Romania precipitation file is not distributed with
# the package or public repository.
#
# Historical pluviograph input:
#
#   dats  = rainfall timestamp
#   cant  = rainfall amount in mm
#
# The historical rainfall record is sparse:
#
#   - not every one-minute timestamp is stored;
#   - long dry periods may be absent entirely;
#   - some zero-rainfall rows may nevertheless be present;
#   - date-only timestamps are interpreted as midnight.
#
# Rfactor uses every available rainfall observation.
#
# During rainfall-event identification, a regular one-minute grid is
# reconstructed only between the first and last positive-rainfall
# observation belonging to each event.
#
# For comparison with RIST fixed-interval calculations, a complete
# one-minute series is created between the first and last available
# source timestamps, with absent timestamps represented by zero
# recorded rainfall.
#
# The use of UTC in this validation is a convention: timestamps are
# interpreted exactly as written in the historical source file, without
# daylight-saving-time transformations.
#
# ==============================================================================


devtools::load_all()


# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

input_file <-
  "sample/1min_vladeasa-1800_15119_1991-1991.csv"

output_dir <-
  "data-raw/real-validation"

dataset_id <-
  "Vladeasa-1800_15119_1991_available"

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
  datetime_col = "dats",
  precip_col = "cant",
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
  "Dataset:               ",
  dataset_id,
  "\n",
  sep = ""
)


cat(
  "First observation:     ",
  format(
    min(rain$datetime),
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ),
  "\n",
  sep = ""
)


cat(
  "Last observation:      ",
  format(
    max(rain$datetime),
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ),
  "\n",
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
  sum(
    rain$precip_mm
  ),
  " mm\n",
  sep = ""
)


cat(
  "Positive observations: ",
  sum(
    rain$precip_mm > 0
  ),
  "\n",
  sep = ""
)


cat(
  "Explicit zero rows:    ",
  sum(
    rain$precip_mm == 0
  ),
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# Describe sparse timestamp structure.
#
# This is diagnostic information only.
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


  largest_gap_min <- max(
    intervals
  )

} else {

  n_gaps <- 0

  missing_intervals <- 0

  largest_gap_min <- 0
}


cat(
  "Timestamp gaps:        ",
  n_gaps,
  "\n",
  sep = ""
)


cat(
  "Absent 1-min rows:     ",
  format(
    missing_intervals,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Largest timestamp gap: ",
  largest_gap_min,
  " minutes\n",
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
# CHECK STORM RECONSTRUCTION
# ==============================================================================

#
# The reconstruction must preserve all positive rainfall supplied by
# the historical source.
#

input_positive_rainfall <- sum(
  rain$precip_mm[
    rain$precip_mm > 0
  ]
)


storm_rainfall <- sum(
  storms$precip_mm
)


if (
  abs(
    input_positive_rainfall -
    storm_rainfall
  ) > 1e-8
) {

  stop(
    paste0(
      "Rainfall total changed during storm reconstruction. ",
      "Input positive rainfall = ",
      input_positive_rainfall,
      " mm; reconstructed storm rainfall = ",
      storm_rainfall,
      " mm."
    )
  )
}


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
# CREATE RIST VALIDATION INPUT
# ==============================================================================

#
# Rfactor processes the sparse historical series directly.
#
# For comparison with RIST fixed-interval mode, construct a complete
# one-minute grid from the first to the last available timestamp.
#
# Timestamps absent from the historical source are assigned 0 mm of
# recorded rainfall.
#
# This dense representation is created only for RIST validation.
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

  Rain = sprintf(
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
  "Dense observations:    ",
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
  "Rainfall total:        ",
  sum(
    dense_precip
  ),
  " mm\n",
  sep = ""
)


cat(
  "Inserted zero rows:    ",
  format(
    length(
      full_grid
    ) -
      nrow(
        rain
      ),
    big.mark = ","
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
