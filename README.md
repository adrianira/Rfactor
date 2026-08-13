
<!-- README.md is generated from README.Rmd. Please edit README.Rmd, not README.md. -->

# Rfactor

<!-- badges: start -->

<!-- badges: end -->

`Rfactor` is an R package for calculating rainfall erosivity from
timestamped precipitation records.

The package identifies independent rainfall events, calculates maximum
rolling rainfall intensities, rainfall kinetic energy, and event EI30
erosivity, and aggregates erosive-event EI30 to monthly or yearly
calendar totals.

These quantities can be used in the derivation of the rainfall-runoff
erosivity factor (R-factor) used by USLE and RUSLE.

`Rfactor` focuses on rainfall erosivity calculations. It does not
calculate the other USLE/RUSLE factors and does not perform spatial
interpolation or mapping.

## Main functions

The public workflow is built around:

- `rf_settings()` — configure rainfall-event separation, omission
  criteria, intensity durations, and kinetic-energy equation;
- `rf_read_rainfall()` — read timestamped precipitation data;
- `rf_validate_rainfall()` — validate rainfall structure and temporal
  alignment;
- `rf_identify_storms()` — identify independent rainfall events;
- `rf_max_intensity()` — calculate maximum rolling rainfall intensity;
- `rf_calculate_energy()` — calculate rainfall kinetic energy;
- `rf_calculate_ei30()` — calculate event energy, intensities, EI30, and
  erosive-event classification;
- `rf_calculate_rfactor()` — aggregate erosive-event EI30 by month or
  year.

## Installation

`Rfactor` is currently under development and is not yet available from
CRAN.

From a local checkout of the package source, install it with:

``` r
devtools::install()
```

Installation instructions for the public Git repository will be added
when the repository address is finalized.

## Quick start

A rainfall file needs a timestamp column and a precipitation column. For
example:

``` text
datetime,precip_mm
2025-07-01 12:00:00,0.0
2025-07-01 12:01:00,0.4
2025-07-01 12:02:00,0.8
2025-07-01 12:03:00,0.3
```

Read and validate the rainfall data:

``` r
library(Rfactor)

rain <- rf_read_rainfall(
  "rainfall.csv",
  datetime_col = "datetime",
  precip_col = "precip_mm",
  tz = "UTC",
  expected_interval_min = 1
)
```

Identify rainfall events:

``` r
storms <- rf_identify_storms(
  rain
)
```

Calculate event rainfall erosivity:

``` r
events <- rf_calculate_ei30(
  storms
)

events[
  ,
  c(
    "event_start",
    "event_end",
    "precip_mm",
    "i30_mm_h",
    "energy_mj_ha",
    "ei30",
    "erosive"
  )
]
```

Aggregate erosive-event EI30 by calendar month or year:

``` r
monthly <- rf_calculate_rfactor(
  events,
  period = "monthly"
)

yearly <- rf_calculate_rfactor(
  events,
  period = "yearly"
)
```

## Calculation settings

Calculation settings are created with `rf_settings()`.

The principal defaults use:

- a 6-hour rainfall-event break;
- precipitation omission below 12.70 mm;
- maximum 15-minute intensity omission below 25.40 mm/h;
- `"all"` omission logic, so both enabled omission conditions must be
  satisfied;
- Brown and Foster (1987) rainfall kinetic energy;
- maximum intensities at 5, 10, 15, 20, 30, and 60 minutes.

The omission comparisons are strict. For example, an event with exactly
12.70 mm precipitation does not satisfy the default precipitation
omission condition, and an event with exactly 25.40 mm/h maximum
15-minute intensity does not satisfy the default intensity omission
condition.

The available kinetic-energy equations are Brown and Foster (1987),
McGregor et al. (1995), and Laws and Parsons (1943).

For example:

``` r
settings <- rf_settings(
  energy_equation = "mcgregor_1995",
  omit_precip = FALSE,
  omit_intensity = FALSE
)

storms <- rf_identify_storms(
  rain,
  settings = settings
)

events <- rf_calculate_ei30(
  storms
)
```

Setting both omission switches to `FALSE` includes every identified
rainfall event.

The RIST storm-break precipitation setting `storm_break_precip_mm` is
retained as configuration metadata. Validation against RIST 3.99.10
fixed-interval input showed that changing this setting did not alter
rainfall-event grouping in the tested cases. In the validated
fixed-interval algorithm used by `Rfactor`, event separation is
controlled by `storm_break_hours`, and any positive rainfall observation
resets the storm-break clock.

## Temporal resolution

Rainfall intensity calculations depend on the temporal resolution of the
precipitation record.

A requested intensity duration must be an exact multiple of the source
interval. For example, 10-minute rainfall data can be used to calculate
10-, 20-, 30-, and 60-minute intensities, but not a true 5- or 15-minute
intensity.

EI30 additionally requires a genuine 30-minute intensity, so the source
interval must divide 30 minutes exactly. Fixed-interval calculations
have been validated against RIST 3.99.10 using 1-, 5-, 10-, 15-, and
30-minute rainfall data.

Rainfall records coarser than 30 minutes cannot provide a genuine I30
and therefore cannot be used to calculate genuine EI30.

Temporal aggregation can change maximum intensity, kinetic energy, and
EI30 even when the total rainfall amount is preserved.

## Sparse and incomplete rainfall records

`Rfactor` does not require every expected timestamp to be present in the
input file.

Gaps are accepted when timestamps remain aligned with the declared base
temporal resolution. The package does not interpolate missing
precipitation and does not assess or correct the climatological
completeness of the rainfall record.

During rainfall-event identification, a regular time grid is
reconstructed only between the first and last positive-rainfall
observation belonging to each event. Expected positions absent from the
supplied record are assigned zero recorded rainfall for the calculation.

An inserted zero is a computational representation of an absent source
position; it does not assert that the corresponding interval was
observed and dry.

Completely absent months or years are not created or imputed.

## Interpreting `R`

`rf_calculate_rfactor()` sums the EI30 values of contributing erosive
events within the requested calendar month or year.

A monthly or yearly value returned by this function is therefore an
erosivity total calculated from the rainfall events available for that
period.

A yearly result is **not automatically the climatological long-term mean
annual R-factor** used by USLE or RUSLE. Deriving a representative
long-term R-factor requires an appropriate multi-year rainfall record
and an assessment of record completeness and representativeness.

Calendar periods represented by rainfall events are retained even when
none of those events are erosive; such periods have `R = 0` and
`n_events = 0`. Completely absent calendar periods are not generated.

## Validation

`Rfactor` has been empirically compared with RIST 3.99.10 using
fixed-interval rainfall input.

Validation includes synthetic tests of rainfall-event separation,
continuous rolling intensities, strict omission thresholds,
single-record events, three kinetic-energy equations, and temporal
resolutions of 1, 5, 10, 15, and 30 minutes.

The package was also tested with real high-resolution and sparse
historical precipitation records from Meteo Romania (National
Meteorological Administration). The original Meteo Romania precipitation
records are not distributed with the package or public repository.

For the synthetic temporal-resolution experiment, results were identical
or very close to RIST. Small RIST-specific differences in reported
precipitation and rolling intensity were observed for some 10- and
15-minute synthetic cases; `Rfactor` preserves the rainfall totals
obtained directly from the supplied source data rather than reproducing
those RIST-specific differences.

## License

`Rfactor` is released under the MIT License.
