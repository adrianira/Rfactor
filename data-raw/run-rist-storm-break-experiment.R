# ==============================================================================
# RIST STORM-BREAK PRECIPITATION EXPERIMENT
# ==============================================================================
#
# Purpose:
#
# Determine how RIST 3.99.10 fixed-interval processing interprets the
# storm-break precipitation setting shown in the graphical interface as:
#
#   "Break storms after 6 hours with less than X mm of precipitation"
#
# The same synthetic 1-minute rainfall file was processed repeatedly in
# RIST while changing only X.
#
# Tested storm-break precipitation settings:
#
#   0.00, 0.20, 1.27, 2.00, and 10.00 mm
#
# Experimental result:
#
# All tested precipitation settings produced identical rainfall-event
# grouping.
#
# For the fixed-interval cases tested here:
#
#   - a gap greater than 6 hours between consecutive positive-rainfall
#     observations starts a new event;
#   - any positive rainfall observation resets the six-hour
#     storm-break clock, regardless of its rainfall amount;
#   - the tested storm-break precipitation setting did not affect
#     event grouping.
#
# Rfactor therefore retains storm_break_precip_mm as RIST configuration
# metadata but does not use it in the validated fixed-interval
# storm-separation algorithm.
#
# IMPORTANT:
# These rainfall data are entirely synthetic.
#
# ==============================================================================
# EXPERIMENTAL DESIGN
#
# CASE 1
#   00:00  5.00 mm
#   06:01  5.00 mm
#
#   Control case with no intermediate positive rainfall.
#
#   Expected and validated:
#   TWO rainfall events.
#
#
# CASE 2
#   00:00  5.00 mm
#   03:00  0.40 mm
#   06:01  5.00 mm
#
#   Tests whether a small intermediate positive rainfall observation
#   resets the break clock.
#
#   Expected and validated:
#   ONE rainfall event.
#
#
# CASE 3
#   00:00  5.00 mm
#   03:00  1.27 mm
#   06:01  5.00 mm
#
#   Intermediate rainfall is exactly equal to the default 1.27-mm
#   RIST precipitation setting.
#
#   Expected and validated:
#   ONE rainfall event.
#
#
# CASE 4
#   00:00  5.00 mm
#   03:00  1.28 mm
#   06:01  5.00 mm
#
#   Intermediate rainfall is just above the default 1.27-mm setting.
#
#   Expected and validated:
#   ONE rainfall event.
#
#
# CASE 5
#   00:00  5.00 mm
#   02:00  0.40 mm
#   04:00  0.40 mm
#   06:01  5.00 mm
#
#   Intermediate total = 0.80 mm.
#
#   This tests whether accumulated rainfall during the potential
#   storm-break period changes grouping.
#
#   Expected and validated:
#   ONE rainfall event.
#
#
# CASE 6
#   00:00  5.00 mm
#   02:00  0.70 mm
#   04:00  0.70 mm
#   06:01  5.00 mm
#
#   Intermediate total = 1.40 mm.
#   Each intermediate observation is below 1.27 mm, while their
#   combined amount is above 1.27 mm.
#
#   This tests individual-record versus accumulated-rainfall
#   interpretations of the RIST precipitation setting.
#
#   Expected and validated:
#   ONE rainfall event.
#
# ==============================================================================


output_dir <-
  "data-raw/rist-storm-break-experiment"


dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# BUILD COMPLETE 1-MINUTE SERIES
# ==============================================================================

start_time <- as.POSIXct(
  "2025-06-30 18:00:00",
  tz = "UTC"
)


end_time <- as.POSIXct(
  "2025-07-06 13:00:00",
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
# HELPER TO ADD RAINFALL
# ==============================================================================

add_rain <- function(
    time,
    amount
) {

  time <- as.POSIXct(
    time,
    tz = "UTC"
  )


  position <- match(
    as.numeric(time),
    as.numeric(datetime)
  )


  if (is.na(position)) {
    stop(
      paste0(
        "Timestamp is outside experiment grid: ",
        time
      )
    )
  }


  precip_mm[position] <<-
    precip_mm[position] +
    amount
}


# ==============================================================================
# CASE 1
#
# No intermediate rainfall
# ==============================================================================

add_rain(
  "2025-07-01 00:00:00",
  5
)

add_rain(
  "2025-07-01 06:01:00",
  5
)


# ==============================================================================
# CASE 2
#
# One intermediate 0.40 mm observation
# ==============================================================================

add_rain(
  "2025-07-02 00:00:00",
  5
)

add_rain(
  "2025-07-02 03:00:00",
  0.40
)

add_rain(
  "2025-07-02 06:01:00",
  5
)


# ==============================================================================
# CASE 3
#
# One intermediate observation exactly equal to 1.27 mm
# ==============================================================================

add_rain(
  "2025-07-03 00:00:00",
  5
)

add_rain(
  "2025-07-03 03:00:00",
  1.27
)

add_rain(
  "2025-07-03 06:01:00",
  5
)


# ==============================================================================
# CASE 4
#
# One intermediate observation just above 1.27 mm
# ==============================================================================

add_rain(
  "2025-07-04 00:00:00",
  5
)

add_rain(
  "2025-07-04 03:00:00",
  1.28
)

add_rain(
  "2025-07-04 06:01:00",
  5
)


# ==============================================================================
# CASE 5
#
# Two small observations:
#
# 0.40 + 0.40 = 0.80 mm
# ==============================================================================

add_rain(
  "2025-07-05 00:00:00",
  5
)

add_rain(
  "2025-07-05 02:00:00",
  0.40
)

add_rain(
  "2025-07-05 04:00:00",
  0.40
)

add_rain(
  "2025-07-05 06:01:00",
  5
)


# ==============================================================================
# CASE 6
#
# Two observations:
#
# 0.70 + 0.70 = 1.40 mm
#
# Each amount < 1.27 mm
# Total amount > 1.27 mm
# ==============================================================================

add_rain(
  "2025-07-06 00:00:00",
  5
)

add_rain(
  "2025-07-06 02:00:00",
  0.70
)

add_rain(
  "2025-07-06 04:00:00",
  0.70
)

add_rain(
  "2025-07-06 06:01:00",
  5
)


# ==============================================================================
# VERIFY INPUT
# ==============================================================================

cat(
  "\nRIST STORM-BREAK PRECIPITATION EXPERIMENT\n",
  "======================\n",
  sep = ""
)


cat(
  "Observations:       ",
  length(datetime),
  "\n",
  sep = ""
)


cat(
  "Total rainfall:     ",
  sum(precip_mm),
  " mm\n",
  sep = ""
)


cat(
  "Positive records:   ",
  sum(precip_mm > 0),
  "\n",
  sep = ""
)


# ==============================================================================
# CREATE FIXED-INTERVAL RIST INPUT
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
  "RIST_storm_break_experiment.txt"
)


data.table::fwrite(
  rist_input,
  file = rist_file,
  sep = ",",
  quote = FALSE
)


# ==============================================================================
# CASE MANIFEST
# ==============================================================================

case_manifest <- data.frame(

  case = 1:6,

  date = as.Date(
    sprintf(
      "2025-07-%02d",
      1:6
    )
  ),

  description = c(
    "5 mm at 00:00; 5 mm at 06:01",
    "5 mm; 0.40 mm at 03:00; 5 mm at 06:01",
    "5 mm; 1.27 mm at 03:00; 5 mm at 06:01",
    "5 mm; 1.28 mm at 03:00; 5 mm at 06:01",
    "5 mm; 0.40 mm at 02:00; 0.40 mm at 04:00; 5 mm at 06:01",
    "5 mm; 0.70 mm at 02:00; 0.70 mm at 04:00; 5 mm at 06:01"
  ),

  expected_events = c(
    2,
    1,
    1,
    1,
    1,
    1
  )
)


manifest_file <- file.path(
  output_dir,
  "RIST_storm_break_experiment_cases.csv"
)


data.table::fwrite(
  case_manifest,
  file = manifest_file
)


cat(
  "\nFILES CREATED\n",
  "=============\n",
  sep = ""
)


cat(
  "RIST input:\n  ",
  rist_file,
  "\n\n",
  sep = ""
)


cat(
  "Case manifest:\n  ",
  manifest_file,
  "\n",
  sep = ""
)
