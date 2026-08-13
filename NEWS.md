# Rfactor 0.1.0

## Initial release

* Added tools for reading and validating timestamped precipitation records,
  including sparse records with temporal gaps.

* Added configurable rainfall-event identification using an elapsed
  storm-break duration.

* Added continuous rolling maximum rainfall-intensity calculations.

* Added rainfall kinetic-energy calculations using:
  - Brown and Foster (1987);
  - McGregor et al. (1995);
  - Laws and Parsons (1943).

* Added event EI30 rainfall-erosivity calculations and configurable
  precipitation- and intensity-based event omission criteria.

* Added monthly and yearly aggregation of contributing event EI30 values.

* Added support for configurable rainfall-intensity durations and
  fixed-interval rainfall resolutions compatible with EI30 calculation.

* Added optional RIST-compatible handling of single-record rainfall events.

* Added empirical validation against RIST 3.99.10 using synthetic
  fixed-interval rainfall data, including:
  - storm-break boundary and intermediate-rainfall experiments;
  - strict omission-threshold tests;
  - kinetic-energy equation comparisons;
  - 1-, 5-, 10-, 15-, and 30-minute temporal-resolution comparisons.

* Added validation with real high-resolution and sparse historical
  precipitation records from Meteo Romania (National Meteorological
  Administration). Original Meteo Romania precipitation records are not
  distributed with the package or public repository.

* Added package documentation, a getting-started vignette, and a methods
  and validation vignette.
