# ==============================================================================
# RIST ENERGY-EQUATION EXPERIMENT
# ==============================================================================
#
# Purpose:
#
# Validate the three rainfall kinetic-energy equations implemented in
# Rfactor against RIST 3.99.10 fixed-interval calculations:
#
#   Brown and Foster (1987)
#   McGregor et al. (1995)
#   Laws and Parsons (1943)
#
# Three synthetic 30-minute constant-intensity rainfall events are used:
#
#   Event 1: 0.10 mm/min =   6 mm/h
#   Event 2: 1.00 mm/min =  60 mm/h
#   Event 3: 2.00 mm/min = 120 mm/h
#
# All rainfall data in this experiment are synthetic.
#
# Experimental results:
#
#   - Brown and Foster calculations agreed with RIST 3.99.10.
#
#   - McGregor calculations agreed with the coefficient 0.082 used by
#     Rfactor. Although the RIST graphical interface displays 0.08,
#     its calculated values matched the 0.082 formulation.
#
#   - Laws and Parsons calculations agreed with
#
#       e = 0.119 + 0.0873 log10(i)
#
#     applied directly to positive rainfall intensity.
#
#   - The 120 mm/h event demonstrated that RIST 3.99.10 did not apply
#     a 76.2 mm/h upper intensity cap in the tested fixed-interval
#     calculation.
#
# ==============================================================================


devtools::load_all()


output_dir <-
  "data-raw/rist-energy-equation-experiment"


dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# COMPLETE 1-MINUTE SERIES
# ==============================================================================

start_time <- as.POSIXct(
  "2025-08-01 00:00:00",
  tz = "UTC"
)


end_time <- as.POSIXct(
  "2025-08-01 14:29:00",
  tz = "UTC"
)


datetime <- seq(
  from = start_time,
  to = end_time,
  by = 60
)


precip_mm <- rep(
  0,
  length(datetime)
)


# ==============================================================================
# HELPER
# ==============================================================================

add_constant_storm <- function(
    start,
    precip_per_min,
    duration_min = 30
) {

  storm_start <- as.POSIXct(
    start,
    tz = "UTC"
  )


  storm_times <- seq(
    from = storm_start,
    by = 60,
    length.out = duration_min
  )


  positions <- match(
    as.numeric(storm_times),
    as.numeric(datetime)
  )


  if (anyNA(positions)) {
    stop(
      "Storm falls outside experiment time grid.",
      call. = FALSE
    )
  }


  precip_mm[positions] <<-
    precip_per_min
}


# ==============================================================================
# THREE TEST RAINFALL EVENTS
# ==============================================================================

# Event 1:
#
# 0.10 mm/min = 6 mm/h
# Total = 3 mm

add_constant_storm(
  start = "2025-08-01 00:00:00",
  precip_per_min = 0.10
)


# Event 2:
#
# 1.00 mm/min = 60 mm/h
# Total = 30 mm

add_constant_storm(
  start = "2025-08-01 07:00:00",
  precip_per_min = 1.00
)


# Event 3:
#
# 2.00 mm/min = 120 mm/h
# Total = 60 mm
#
# This case was included specifically to test whether RIST applies
# an upper intensity cap to the Laws-Parsons equation.
#
# Validation showed that RIST 3.99.10 applies the equation directly
# at 120 mm/h, without a 76.2 mm/h cap.

add_constant_storm(
  start = "2025-08-01 14:00:00",
  precip_per_min = 2.00
)


# ==============================================================================
# VERIFY INPUT
# ==============================================================================

cat(
  "\nENERGY-EQUATION EXPERIMENT\n",
  "==========================\n",
  sep = ""
)


cat(
  "Total rainfall: ",
  sum(precip_mm),
  " mm\n",
  sep = ""
)


cat(
  "Positive records: ",
  sum(precip_mm > 0),
  "\n\n",
  sep = ""
)


stopifnot(
  abs(
    sum(precip_mm) - 93
  ) < 1e-12
)


stopifnot(
  sum(
    precip_mm > 0
  ) == 90
)


# ==============================================================================
# RIST INPUT
# ==============================================================================

rist_input <- data.frame(

  An = as.integer(
    format(
      datetime,
      "%Y",
      tz = "UTC"
    )
  ),

  Luna = as.integer(
    format(
      datetime,
      "%m",
      tz = "UTC"
    )
  ),

  Zi = as.integer(
    format(
      datetime,
      "%d",
      tz = "UTC"
    )
  ),

  Ora = as.integer(
    format(
      datetime,
      "%H",
      tz = "UTC"
    )
  ),

  Minut = as.integer(
    format(
      datetime,
      "%M",
      tz = "UTC"
    )
  ),

  Secunda = as.integer(
    format(
      datetime,
      "%S",
      tz = "UTC"
    )
  ),

  Rain = sprintf(
    "%.3f",
    precip_mm
  )
)


rist_file <- file.path(
  output_dir,
  "RIST_energy_equation_experiment.txt"
)


data.table::fwrite(
  rist_input,
  file = rist_file,
  sep = ",",
  quote = FALSE
)


# ==============================================================================
# RFACTOR INPUT
# ==============================================================================

rainfall <- data.frame(
  datetime = datetime,
  precip_mm = precip_mm
)


# ==============================================================================
# CALCULATE RFACTOR RESULTS FOR EACH EQUATION
# ==============================================================================

equations <- c(
  "brown_foster_1987",
  "mcgregor_1995",
  "laws_parsons_1943"
)


rfactor_results <- vector(
  "list",
  length(equations)
)


for (
  i in seq_along(
    equations
  )
) {

  equation <- equations[i]


  settings <- rf_settings(
    energy_equation = equation,

    # Include every rainfall event in this experiment.
    omit_precip = FALSE,
    omit_intensity = FALSE
  )


  storms <- rf_identify_storms(
    rainfall,
    settings = settings,
    interval_min = 1
  )


  events <- rf_calculate_ei30(
    storms,
    settings = settings,
    interval_min = 1
  )


  events$energy_equation <-
    equation


  rfactor_results[[i]] <-
    events
}


rfactor_results <- do.call(
  rbind,
  rfactor_results
)


rownames(
  rfactor_results
) <- NULL


# ==============================================================================
# EXPORT RFACTOR REFERENCE RESULTS
# ==============================================================================

rfactor_file <- file.path(
  output_dir,
  "Rfactor_energy_equation_results.csv"
)


data.table::fwrite(
  rfactor_results,
  file = rfactor_file
)


# ==============================================================================
# EXPERIMENT MANIFEST
# ==============================================================================

manifest <- data.frame(

  event = 1:3,

  start = c(
    "2025-08-01 00:00:00",
    "2025-08-01 07:00:00",
    "2025-08-01 14:00:00"
  ),

  precip_per_min_mm = c(
    0.10,
    1.00,
    2.00
  ),

  intensity_mm_h = c(
    6,
    60,
    120
  ),

  total_precip_mm = c(
    3,
    30,
    60
  )
)


manifest_file <- file.path(
  output_dir,
  "energy_equation_experiment_cases.csv"
)


data.table::fwrite(
  manifest,
  file = manifest_file
)


# ==============================================================================
# REPORT
# ==============================================================================

cat(
  "RIST input:\n  ",
  rist_file,
  "\n\n",
  sep = ""
)


cat(
  "Rfactor results:\n  ",
  rfactor_file,
  "\n\n",
  sep = ""
)


cat(
  "Case manifest:\n  ",
  manifest_file,
  "\n\n",
  sep = ""
)


print(
  rfactor_results[
    ,
    c(
      "storm_id",
      "precip_mm",
      "energy_mj_ha",
      "i30_mm_h",
      "ei30",
      "energy_equation"
    )
  ]
)
