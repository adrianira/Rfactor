# ------------------------------------------------------------------
# Internal rainfall unit-energy functions
#
# All functions:
#
#   input  = rainfall intensity in mm/h
#   output = unit rainfall energy in MJ/ha/mm
# ------------------------------------------------------------------


# Brown and Foster (1987)
#
# e = 0.29 [1 - 0.72 exp(-0.05 i)]

.rf_unit_energy_brown_foster_1987 <- function(
    intensity_mm_h
) {

  0.29 * (
    1 -
      0.72 *
      exp(
        -0.05 * intensity_mm_h
      )
  )
}


# McGregor et al. (1995)
#
# e = 0.29 [1 - 0.72 exp(-0.082 i)]
#
# USDA RUSLE2 documentation gives a coefficient of 0.082.
# The RIST 3.99.10 graphical interface displays this coefficient
# as 0.08, but dedicated validation against RIST showed that its
# calculated energy values agree with the 0.082 coefficient.

.rf_unit_energy_mcgregor_1995 <- function(
    intensity_mm_h
) {

  0.29 * (
    1 -
      0.72 *
      exp(
        -0.082 * intensity_mm_h
      )
  )
}


# Laws and Parsons (1943), as used by RIST 3.99.10
#
# e = 0.119 + 0.0873 log10(i)
#
# Dedicated validation against RIST 3.99.10 showed that the
# equation is applied directly above 76.2 mm/h. In particular,
# a constant 120 mm/h storm was calculated without an intensity
# cap.
#
# At zero rainfall intensity the logarithmic equation is undefined.
# A zero-rainfall interval contributes no kinetic energy, so its
# unit energy is explicitly set to zero.

.rf_unit_energy_laws_parsons_1943 <- function(
    intensity_mm_h
) {

  unit_energy <- numeric(
    length(
      intensity_mm_h
    )
  )


  positive <- intensity_mm_h > 0


  unit_energy[
    positive
  ] <-
    0.119 +
    0.0873 *
    log10(
      intensity_mm_h[
        positive
      ]
    )


  unit_energy
}


# ------------------------------------------------------------------
# Internal equation dispatcher
# ------------------------------------------------------------------

.rf_unit_energy <- function(
    intensity_mm_h,
    energy_equation
) {

  switch(
    energy_equation,

    brown_foster_1987 =
      .rf_unit_energy_brown_foster_1987(
        intensity_mm_h
      ),

    mcgregor_1995 =
      .rf_unit_energy_mcgregor_1995(
        intensity_mm_h
      ),

    laws_parsons_1943 =
      .rf_unit_energy_laws_parsons_1943(
        intensity_mm_h
      )
  )
}


#' Calculate rainfall kinetic energy
#'
#' Calculates total rainfall kinetic energy for a rainfall event from
#' fixed-interval precipitation depths.
#'
#' Rainfall intensity for each observation interval is calculated as
#'
#' \deqn{
#' i_k = P_k \frac{60}{\Delta t}
#' }
#'
#' where `P_k` is interval precipitation in mm and `\Delta t` is the
#' interval duration in minutes.
#'
#' Three kinetic-energy equations are available.
#'
#' @section Brown and Foster (1987):
#'
#' \deqn{
#' e = 0.29 [1 - 0.72 \exp(-0.05 i)]
#' }
#'
#' @section McGregor et al. (1995):
#'
#' \deqn{
#' e = 0.29 [1 - 0.72 \exp(-0.082 i)]
#' }
#'
#' The coefficient `0.082` is used. Although the RIST 3.99.10
#' graphical interface displays this coefficient as `0.08`,
#' dedicated validation showed that RIST energy calculations agree
#' with the `0.082` formulation.
#'
#' @section Laws and Parsons (1943):
#'
#' \deqn{
#' e = 0.119 + 0.0873 \log_{10}(i)
#' }
#'
#' For compatibility with the behaviour validated against RIST
#' 3.99.10, this equation is applied directly to positive rainfall
#' intensities without an upper intensity cap. Validation included
#' rainfall intensity of 120 mm/h.
#'
#' The logarithmic equation is undefined at zero intensity.
#' Zero-rainfall intervals are therefore assigned zero unit energy
#' and contribute zero kinetic energy.
#'
#' @section Event energy:
#'
#' For all equations, `e` is unit rainfall energy in MJ/ha/mm and
#' `i` is rainfall intensity in mm/h. Total rainfall energy is
#'
#' \deqn{
#' E = \sum_k e_k P_k
#' }
#'
#' where `E` is total event energy in MJ/ha.
#'
#' `precip_mm` is assumed to represent consecutive observations at
#' the fixed interval specified by `interval_min`. This function does
#' not contain timestamps and therefore does not detect temporal gaps.
#' In the normal `Rfactor` workflow, [rf_identify_storms()] prepares
#' the regular within-event rainfall sequence before energy is
#' calculated.
#'
#' @param precip_mm Numeric vector containing precipitation depth, in
#'   mm, for each consecutive fixed observation interval. Values must
#'   be finite, non-missing, and non-negative.
#'
#' @param interval_min Positive finite number giving the duration of
#'   each observation interval, in minutes. Default is `1`.
#'
#' @param energy_equation Character string selecting the kinetic-energy
#'   equation. One of:
#'
#'   - `"brown_foster_1987"`;
#'   - `"mcgregor_1995"`;
#'   - `"laws_parsons_1943"`.
#'
#'   Default is `"brown_foster_1987"`.
#'
#' @return Total rainfall kinetic energy for the supplied event, in
#'   MJ/ha.
#'
#' @examples
#' # Thirty minutes at 1 mm/min:
#' rf_calculate_energy(
#'   precip_mm = rep(1, 30),
#'   interval_min = 1
#' )
#'
#' # The same rainfall using the McGregor equation
#' rf_calculate_energy(
#'   precip_mm = rep(1, 30),
#'   interval_min = 1,
#'   energy_equation = "mcgregor_1995"
#' )
#'
#' # Ten-minute fixed-interval rainfall data
#' rf_calculate_energy(
#'   precip_mm = c(2, 5, 3),
#'   interval_min = 10,
#'   energy_equation = "laws_parsons_1943"
#' )
#'
#' @export


rf_calculate_energy <- function(
    precip_mm,
    interval_min = 1,
    energy_equation = "brown_foster_1987"
) {

  # ------------------------------------------------------------------
  # Input checks
  # ------------------------------------------------------------------

  if (!is.numeric(precip_mm)) {
    stop(
      "precip_mm must be numeric.",
      call. = FALSE
    )
  }


  if (length(precip_mm) == 0) {
    stop(
      "precip_mm must contain at least one value.",
      call. = FALSE
    )
  }


  if (anyNA(precip_mm)) {
    stop(
      "precip_mm contains missing values.",
      call. = FALSE
    )
  }


  if (any(!is.finite(precip_mm))) {
    stop(
      "precip_mm contains non-finite values.",
      call. = FALSE
    )
  }


  if (any(precip_mm < 0)) {
    stop(
      "precip_mm cannot contain negative values.",
      call. = FALSE
    )
  }


  if (
    length(interval_min) != 1 ||
    !is.numeric(interval_min) ||
    !is.finite(interval_min) ||
    interval_min <= 0
  ) {
    stop(
      "interval_min must be one positive finite value.",
      call. = FALSE
    )
  }


  energy_equation <- match.arg(
    energy_equation,
    choices = c(
      "brown_foster_1987",
      "mcgregor_1995",
      "laws_parsons_1943"
    )
  )


  # ------------------------------------------------------------------
  # Convert interval rainfall depth to rainfall intensity
  #
  # Example:
  #
  # 1 mm in 1 minute = 60 mm/h
  # ------------------------------------------------------------------

  intensity_mm_h <-
    precip_mm *
    60 /
    interval_min


  # ------------------------------------------------------------------
  # Calculate unit rainfall energy using the selected equation
  # ------------------------------------------------------------------

  unit_energy <- .rf_unit_energy(
    intensity_mm_h =
      intensity_mm_h,

    energy_equation =
      energy_equation
  )


  # ------------------------------------------------------------------
  # Total event energy
  # ------------------------------------------------------------------

  energy_mj_ha <- sum(
    unit_energy *
      precip_mm
  )


  energy_mj_ha
}
