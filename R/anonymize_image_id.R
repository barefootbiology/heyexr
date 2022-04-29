#' Anonymize the image ID in a volume object.
#'
#' \code{anonymize_image_id} changes the 'id' (an internal identifier used by
#' the Heidelberg database) and 'reference_id' (if present) values in a
#' volume object header.
#'
#' @param header The header list from a volume object.
#'
#' @return A new header list with the image ID and reference IDs values changed.
#'
#' @keywords internal
#' @importFrom digest digest
#' @importFrom stringr str_trunc
anonymize_image_id <- function(header) {
  new_id_0 <- digest::digest(header$id, algo = "md5")
  header$id <- str_trunc(new_id_0, width = 16, side = "right", ellipsis = "")

  if(header$reference_id != "") {
      new_reference_id_0 <-
          digest::digest(header$reference_id, algo = "md5")

      header$reference_id <-
          str_trunc(new_reference_id_0, width = 16, side = "right", ellipsis = "")
  }

  header
}
