# ------------------------------------------------------------------
# Internal numerical comparison helpers
# ------------------------------------------------------------------


# Strict less-than comparison with floating-point tolerance.
#
# Used for storm-omission thresholds where equality must not satisfy
# the omission condition. The tolerance prevents values that differ
# from the threshold only because of floating-point representation
# from being classified as strictly smaller.

.rf_less_than <- function(
    x,
    threshold,
    tolerance = 1e-9
) {

  x < (threshold - tolerance)
}
