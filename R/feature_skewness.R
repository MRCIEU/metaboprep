#' @title Estimate Feature Skewness
#' @description
#' Estimate per-feature skewness and optionally flag features beyond a user-defined
#' skewness threshold.
#' @param data matrix, a numeric matrix with samples in rows and features in columns.
#' @param threshold numeric, optional skewness threshold. If `NULL`, only skewness is
#' returned and no exclusion flag is calculated.
#' @param direction character, direction of skewness to flag. One of `"left"`,
#' `"right"`, or `"both"`.
#'
#' @return data.frame with columns `feature_id`, `skew`, and
#' `exclude_by_skewness` (logical; `NA` if `threshold = NULL`).
#'
#' @importFrom psych describe
#' @export
feature_skewness <- function(data, threshold = NULL, direction = "left") {
  stopifnot("data must be a matrix" = is.matrix(data))
  direction <- match.arg(direction, choices = c("left", "right", "both"))
  stopifnot("threshold must be NULL or a non-negative scalar" =
              is.null(threshold) || (is.numeric(threshold) && length(threshold) == 1 && !is.na(threshold) && threshold >= 0))

  desc <- psych::describe(data)
  out <- data.frame(
    feature_id = rownames(desc),
    skew = as.numeric(desc[, "skew"])
  )

  if (is.null(threshold)) {
    out$exclude_by_skewness <- NA
  } else {
    out$exclude_by_skewness <- switch(
      direction,
      "left" = out$skew <= -threshold,
      "right" = out$skew >= threshold,
      "both" = abs(out$skew) >= threshold
    )
  }

  out
}
