#' Anonymize dates in a volume object.
#'
#' \code{anonymize_dates} sets the date of birth ('dob') to a given reference
#' point then adjusts the 'exam_time' and 'visit_date' values to preserve the
#' age at visit.
#'
#' @param header The header list from a volume object.
#' @param anon_dob The new date of birth. Defaults to the origin in Microsoft's
#'     DATE specification as used by the 'dob' and 'visit_date' values in the
#'     Heidelberg VOL file specification (HSF-OCT-101).
#'
#' @return A new header list with the dates adjusted.
#'
#' @keywords internal
anonymize_dates <- function(header, anon_dob = as.POSIXct(0, origin = "1899-12-30", tz = "UTC")) {
  # The time encoded in 'exam_time' and 'visit_date' may be a few minutes off.
  age_at_exam <- header$exam_time - header$dob
  age_at_visit <- header$visit_date - header$dob

  header$dob <- anon_dob

  header$exam_time <- anon_dob + age_at_exam

  header$visit_date <- anon_dob + age_at_visit

  header
}
