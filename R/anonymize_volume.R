#' Anonymize volume objects.
#'
#' \code{anonymize_volume} updates the patient ID, date of birth, and date
#' fields specified in Heidelberg's HSF-OCT-101 file format specification.
#' NOTE: This function will not catch identifying data if it is included in the
#' 'spare' bytes or other fields in the volume object. We strongly recommend
#' that you inspect your own VOL files for other potentially identifying
#' fields not documented in HSF-OCT-101.
#'
#' @param volume A volume object
#' @param anon_pid A new internal patient ID.
#' @param anon_patient_id A new patient ID.
#' @param anon_dob The new date of birth. Defaults to the origin in Microsoft's
#'     DATE specification as used by the 'dob' and 'visit_date' values in the
#'     Heidelberg VOL file specification (HSF-OCT-101).
#'
#' @return A new volume object with the identifiers and dates adjusted.
#'
#' @export
anonymize_volume <- function(volume, pid, patient_id = pid,
                               anon_dob = as.POSIXct(0, origin = "1899-12-30", tz = "UTC")) {
  volume$header <- anonymize_identity(header = volume$header, pid = pid, patient_id = patient_id)
  volume$header <- anonymize_dates(header = volume$header, anon_dob = anon_dob)

  volume
}
