# ==============================================================================
# RIST TEMPORAL-RESOLUTION VALIDATION
# ==============================================================================
#
# Purpose:
#
# Validate Rfactor against RIST 3.99.10 using synthetic fixed-interval
# rainfall data at:
#
#   1 minute
#   5 minutes
#   10 minutes
#   15 minutes
#   30 minutes
#
# The same underlying synthetic 1-minute rainfall record is aggregated
# to each temporal resolution before being supplied independently to
# Rfactor and RIST.
#
# The comparison includes:
#
#   rainfall-event identification
#   event precipitation
#   event duration
#   rainfall kinetic energy
#   I30
#   I60
#   EI30
#
# Brown and Foster (1987) is used throughout so that temporal
# resolution is the principal methodological variable.
#
# Event omission is disabled so that every identified rainfall event
# is included.
#
# Experimental results:
#
#   - Rfactor produced identical or very close results to RIST 3.99.10
#     at all five tested temporal resolutions.
#
#   - The 1-, 5-, and 30-minute synthetic cases agreed with the
#     corresponding RIST results to the displayed precision.
#
#   - Small RIST-specific differences were observed for the variable
#     event at 10- and 15-minute resolution. RIST reported slightly
#     different precipitation totals and rolling intensities, while
#     Rfactor preserved the rainfall totals obtained directly from the
#     temporal aggregation.
#
#   - These RIST-specific reporting differences are not reproduced by
#     Rfactor.
#
#   - Temporal aggregation can change rainfall intensity, kinetic
#     energy, and EI30 even when total rainfall is preserved.
#
# This experiment validates the tested fixed intervals only. It does
# not imply that every temporal resolution between 1 and 30 minutes is
# compatible with EI30. A genuine I30 calculation requires the rainfall
# interval to divide 30 minutes exactly.
#
# ==============================================================================


devtools::load_all()


output_dir <-
  "data-raw/rist-resolution-experiment"


dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# MASTER 1-MINUTE RAINFALL SERIES
# ==============================================================================

start_time <- as.POSIXct(
  "2025-09-01 00:00:00",
  tz = "UTC"
)


end_time <- as.POSIXct(
  "2025-09-01 23:59:00",
  tz = "UTC"
)


datetime_1min <- seq(
  from = start_time,
  to = end_time,
  by = 60
)


precip_1min <- rep(
  0,
  length(
    datetime_1min
  )
)


# ==============================================================================
# HELPER: ADD CONSTANT 1-MINUTE RAINFALL
# ==============================================================================

add_rain <- function(
    start,
    duration_min,
    precip_per_min
) {

  event_start <- as.POSIXct(
    start,
    tz = "UTC"
  )


  first_position <- match(
    as.numeric(
      event_start
    ),
    as.numeric(
      datetime_1min
    )
  )


  if (is.na(first_position)) {
    stop(
      paste0(
        "Event start is outside the master time series: ",
        start
      ),
      call. = FALSE
    )
  }


  positions <- seq(
    from = first_position,
    length.out = duration_min
  )


  if (
    max(
      positions
    ) >
    length(
      precip_1min
    )
  ) {
    stop(
      "Event extends beyond the master time series.",
      call. = FALSE
    )
  }


  precip_1min[
    positions
  ] <<-
    precip_per_min
}


# ==============================================================================
## EVENT 1
#
# Constant low-intensity rainfall event
#
# 60 minutes at:
#
#   0.10 mm/min
#   = 6 mm/h
#
# Total rainfall:
#
#   6 mm
#
# This event should produce essentially the same calculation at every
# tested resolution because its intensity is constant.
# ==============================================================================

add_rain(
  start = "2025-09-01 03:00:00",
  duration_min = 60,
  precip_per_min = 0.10
)


# ==============================================================================
# EVENT 2
#
# Variable-intensity rainfall event lasting 60 minutes.
#
# 15:00 - 15:14
#   0.20 mm/min = 12 mm/h
#
# 15:15 - 15:29
#   1.00 mm/min = 60 mm/h
#
# 15:30 - 15:44
#   0.40 mm/min = 24 mm/h
#
# 15:45 - 15:59
#   0.80 mm/min = 48 mm/h
#
# Total:
#
#   3 + 15 + 6 + 12
#   = 36 mm
#
# At fine resolutions, the maximum 30-minute window consists of:
#
#   60 mm/h + 24 mm/h
#
# giving:
#
#   I30 = 42 mm/h
#
# Coarser resolutions intentionally smooth this pattern.
# ==============================================================================

add_rain(
  start = "2025-09-01 15:00:00",
  duration_min = 15,
  precip_per_min = 0.20
)


add_rain(
  start = "2025-09-01 15:15:00",
  duration_min = 15,
  precip_per_min = 1.00
)


add_rain(
  start = "2025-09-01 15:30:00",
  duration_min = 15,
  precip_per_min = 0.40
)


add_rain(
  start = "2025-09-01 15:45:00",
  duration_min = 15,
  precip_per_min = 0.80
)


# ==============================================================================
# VERIFY MASTER DATA
# ==============================================================================

stopifnot(
  length(
    datetime_1min
  ) == 1440
)


stopifnot(
  abs(
    sum(
      precip_1min
    ) -
      42
  ) < 1e-12
)


cat(
  "\nTEMPORAL-RESOLUTION EXPERIMENT\n",
  "==============================\n\n",
  sep = ""
)


cat(
  "Master observations: ",
  length(
    precip_1min
  ),
  "\n",
  sep = ""
)


cat(
  "Master precipitation: ",
  sum(
    precip_1min
  ),
  " mm\n\n",
  sep = ""
)


# ==============================================================================
# HELPER: AGGREGATE 1-MINUTE DATA
#
# All tested resolutions divide evenly into the 1440-minute day.
#
# Timestamps represent the beginning of each fixed interval.
# ==============================================================================

aggregate_resolution <- function(
    interval_min
) {

  if (
    length(
      precip_1min
    ) %%
    interval_min !=
    0
  ) {
    stop(
      "Master data length is not divisible by interval_min.",
      call. = FALSE
    )
  }


  precip_matrix <- matrix(
    precip_1min,
    nrow = interval_min
  )


  aggregated_precip <- colSums(
    precip_matrix
  )


  aggregated_datetime <-
    datetime_1min[
      seq(
        from = 1,
        to = length(
          datetime_1min
        ),
        by = interval_min
      )
    ]


  data.frame(
    datetime =
      aggregated_datetime,

    precip_mm =
      aggregated_precip
  )
}


# ==============================================================================
# HELPER: INTENSITY DURATIONS COMPATIBLE WITH EACH RESOLUTION
#
# Every requested duration must be an exact multiple of interval_min.
#
# I30 is mandatory because EI30 requires it.
# ==============================================================================

compatible_durations <- function(
    interval_min
) {

  candidate_durations <- c(
    5,
    10,
    15,
    20,
    30,
    60
  )


  result <-
    candidate_durations[
      candidate_durations %%
        interval_min ==
        0
    ]


  if (
    !30 %in%
    result
  ) {
    stop(
      paste0(
        "A ",
        interval_min,
        "-minute interval cannot support I30."
      ),
      call. = FALSE
    )
  }


  result
}


# ==============================================================================
# RESOLUTIONS TO VALIDATE
# ==============================================================================

resolutions <- c(
  1,
  5,
  10,
  15,
  30
)


# ==============================================================================
# EXPECTED RFACTOR I30 FOR THE VARIABLE EVENT
#
# These values follow directly from aggregation of the synthetic master
# rainfall record and are the expected Rfactor values:
#
# 1 min   -> 42 mm/h
# 5 min   -> 42 mm/h
# 10 min  -> 40 mm/h
# 15 min  -> 42 mm/h
# 30 min  -> 36 mm/h
#
# The differences are intentional and demonstrate information loss
# caused by temporal aggregation.
#
# RIST 3.99.10 produced small additional reporting differences for the
# 10- and 15-minute versions of this synthetic variable event.
# ==============================================================================

expected_variable_i30 <- c(
  `1` = 42,
  `5` = 42,
  `10` = 40,
  `15` = 42,
  `30` = 36
)


# ==============================================================================
# STORAGE FOR RFACTOR RESULTS
# ==============================================================================

result_list <- vector(
  "list",
  length(
    resolutions
  )
)


# ==============================================================================
# PROCESS EACH RESOLUTION
# ==============================================================================

for (
  resolution_index in
  seq_along(
    resolutions
  )
) {

  interval_min <-
    resolutions[
      resolution_index
    ]


  cat(
    "Processing ",
    interval_min,
    "-minute data...\n",
    sep = ""
  )


  # --------------------------------------------------------------------------
  # Aggregate master rainfall
  # --------------------------------------------------------------------------

  rainfall <-
    aggregate_resolution(
      interval_min
    )


  stopifnot(
    abs(
      sum(
        rainfall$precip_mm
      ) -
        42
    ) < 1e-12
  )


  # --------------------------------------------------------------------------
  # Export package-native rainfall CSV
  #
  # This allows the public rf_read_rainfall() workflow itself to be tested.
  # --------------------------------------------------------------------------

  native_file <- file.path(
    output_dir,
    sprintf(
      "Rfactor_resolution_%02dmin.csv",
      interval_min
    )
  )


  native_export <- data.frame(

    datetime =
      format(
        rainfall$datetime,
        "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),

    precip_mm =
      rainfall$precip_mm
  )


  data.table::fwrite(
    native_export,
    file = native_file
  )


  # --------------------------------------------------------------------------
  # Read through public package reader
  # --------------------------------------------------------------------------

  rainfall_read <-
    rf_read_rainfall(
      native_file,
      datetime_col = "datetime",
      precip_col = "precip_mm",
      tz = "UTC",
      expected_interval_min =
        interval_min
    )


  # --------------------------------------------------------------------------
  # Settings
  #
  # Disable omission so that every identified rainfall event is compared
  # with RIST.
  # --------------------------------------------------------------------------

  settings <- rf_settings(

    energy_equation =
      "brown_foster_1987",

    intensity_durations_min =
      compatible_durations(
        interval_min
      ),

    omit_precip =
      FALSE,

    omit_intensity =
      FALSE,

    single_record_energy =
      "calculate"
  )


  # --------------------------------------------------------------------------
  # Identify rainfall events
  # --------------------------------------------------------------------------

  storms <- rf_identify_storms(
    rainfall_read,
    settings = settings
  )


  # --------------------------------------------------------------------------
  # Calculate event erosivity
  # --------------------------------------------------------------------------

  events <- rf_calculate_ei30(
    storms,
    settings = settings
  )


  # --------------------------------------------------------------------------
  # Basic validation
  # --------------------------------------------------------------------------

  stopifnot(
    nrow(
      events
    ) == 2
  )


  stopifnot(
    abs(
      sum(
        events$precip_mm
      ) -
        42
    ) < 1e-10
  )


  stopifnot(
    all(
      abs(
        events$duration_min -
          60
      ) <
        1e-10
    )
  )


  # Event 1:
  # constant 6 mm/h rainfall

  stopifnot(
    abs(
      events$i30_mm_h[1] -
        6
    ) <
      1e-10
  )


  # Event 2:
  # expected I30 depends on source resolution.

  stopifnot(
    abs(
      events$i30_mm_h[2] -
        expected_variable_i30[
          as.character(
            interval_min
          )
        ]
    ) <
      1e-10
  )


  # --------------------------------------------------------------------------
  # Standardized summary
  #
  # Only intensity measures available at ALL tested resolutions are
  # retained here: I30 and I60.
  # --------------------------------------------------------------------------

  result_list[[
    resolution_index
  ]] <- data.frame(

    resolution_min =
      interval_min,

    storm_id =
      events$storm_id,

    event_start =
      events$event_start,

    event_end =
      events$event_end,

    duration_min =
      events$duration_min,

    precip_mm =
      events$precip_mm,

    energy_mj_ha =
      events$energy_mj_ha,

    i30_mm_h =
      events$i30_mm_h,

    i60_mm_h =
      events$i60_mm_h,

    ei30 =
      events$ei30
  )


  # --------------------------------------------------------------------------
  # Create RIST fixed-interval input
  # --------------------------------------------------------------------------

  rist_input <- data.frame(

    An = as.integer(
      format(
        rainfall$datetime,
        "%Y",
        tz = "UTC"
      )
    ),

    Luna = as.integer(
      format(
        rainfall$datetime,
        "%m",
        tz = "UTC"
      )
    ),

    Zi = as.integer(
      format(
        rainfall$datetime,
        "%d",
        tz = "UTC"
      )
    ),

    Ora = as.integer(
      format(
        rainfall$datetime,
        "%H",
        tz = "UTC"
      )
    ),

    Minut = as.integer(
      format(
        rainfall$datetime,
        "%M",
        tz = "UTC"
      )
    ),

    Secunda = as.integer(
      format(
        rainfall$datetime,
        "%S",
        tz = "UTC"
      )
    ),

    Rain = sprintf(
      "%.6f",
      rainfall$precip_mm
    )
  )


  rist_file <- file.path(
    output_dir,
    sprintf(
      "RIST_resolution_%02dmin.txt",
      interval_min
    )
  )


  data.table::fwrite(
    rist_input,
    file = rist_file,
    sep = ",",
    quote = FALSE
  )
}


# ==============================================================================
# COMBINE RFACTOR RESULTS
# ==============================================================================

rfactor_results <- do.call(
  rbind,
  result_list
)


rownames(
  rfactor_results
) <- NULL


# ==============================================================================
# EXPORT RFACTOR SUMMARY
# ==============================================================================

rfactor_results_file <- file.path(
  output_dir,
  "Rfactor_resolution_results.csv"
)


data.table::fwrite(
  rfactor_results,
  file = rfactor_results_file
)


# ==============================================================================
# EXPERIMENT MANIFEST
# ==============================================================================

manifest <- data.frame(

  resolution_min =
    resolutions,

  calculated_intensities =
    vapply(
      resolutions,
      function(x) {

        paste(
          compatible_durations(
            x
          ),
          collapse = ", "
        )
      },
      character(1)
    ),

  expected_variable_storm_i30_mm_h =
    unname(
      expected_variable_i30[
        as.character(
          resolutions
        )
      ]
    )
)


manifest_file <- file.path(
  output_dir,
  "resolution_experiment_manifest.csv"
)


data.table::fwrite(
  manifest,
  file = manifest_file
)


# ==============================================================================
# REPORT
# ==============================================================================

cat(
  "\nRFACTOR RESULTS\n",
  "===============\n\n",
  sep = ""
)


print(
  rfactor_results
)


cat(
  "\nFiles written to:\n  ",
  output_dir,
  "\n\n",
  sep = ""
)


cat(
  "Rfactor summary:\n  ",
  rfactor_results_file,
  "\n\n",
  sep = ""
)


cat(
  "RIST files:\n",
  sep = ""
)


for (
  interval_min in
  resolutions
) {

  cat(
    "  ",
    sprintf(
      "RIST_resolution_%02dmin.txt",
      interval_min
    ),
    "\n",
    sep = ""
  )
}
