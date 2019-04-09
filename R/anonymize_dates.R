#' Anonymize dates in a volume object.
#'
#' \code{anonymize_dates} sets the date of birth ('dob') to a given reference
#' point then adjusts the 'exam_time' and 'visit_date' values to preserve the
#' age at visit.
#'
#' @param header The header list from a volume object.
#' @param anon_dob The new date of birth. Must be at or after the UNIX date-time
#'     origin of 1970-01-01 UTC (the default value).
#'
#' @return A new header list with the dates adjusted.
#'
#' @importFrom lubridate as.duration
#' @keywords internal
anonymize_dates <- function(header, anon_dob = as.POSIXct(0, origin = "1970-01-01", tz = "UTC")) {
  if(anon_dob < lubridate::origin) {
      stop("Must specify anon_dob to be at or after the UNIX time origin ", as.POSIXct(0, origin = "1970-01-01", tz = "UTC"))
  }

  # The time encoded in 'exam_time' and 'visit_date' may be a few minutes off.
  age_at_exam <- as.duration(header$exam_time - header$dob)
  age_at_visit <- as.duration(header$visit_date - header$dob)

  header$dob <- anon_dob

  header$exam_time <- anon_dob + age_at_exam

  header$visit_date <- anon_dob + age_at_visit

  header
}
