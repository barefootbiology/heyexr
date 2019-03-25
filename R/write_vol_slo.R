#' Write the VOL SLO image
#'
#' Writes the SLO information from a Heidelberg Spectralis VOL file.
#'
#' @param con connection to a VOL file
#' @param slo SLO data matrix
#'
#' @export
write_vol_slo <- function(vol_con, slo_image) {

    writeBin(c(slo_image), con = vol_con, size = 1, endian = "little")

}
