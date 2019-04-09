#' Anonymize identity in a volume object.
#'
#' \code{anonymize_identity} changes the 'pid' (an internal identifier used by
#' the Heidelberg database) and 'patient_id' (typically a hospital number, e.g.,
#' medical record number or MRN) values in a
#' volume object header. NOTE: By default the patient's name is part of the
#' file name, so if you are writing out the volume object data to a file,
#' remember to rename the file accordingly.
#'
#' @param header The header list from a volume object.
#' @param anon_pid A new internal patient ID.
#' @param anon_patient_id A new patient ID.
#'
#' @return A new header list with the ID values changed.
#'
#' @keywords internal
anonymize_identity <- function(header, pid, patient_id = pid) {
  header$pid <- pid
  header$patient_id <- patient_id

  header
}
