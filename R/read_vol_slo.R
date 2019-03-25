#' Read the VOL SLO image
#'
#' Read the SLO data from a Heidelberg Spectralis VOL file.
#'
#' @param con connection to a VOL file
#' @param header previously read header information
#'
#' @return a matrix containing the SLO image
#'
#' @export
read_vol_slo <- function(vol_con, header) {
    # Code based on these two projects:
    #
    # https://github.com/halirutan/HeyexImport
    # http://rsb.info.nih.gov/ij/plugins/heyex/index.html

    slo_data <- readBin(vol_con, integer(), size = 1,
                         n = header$size_x_slo * header$size_y_slo,
                         endian = "little", signed = FALSE)

    matrix(slo_data, nrow = header$size_x_slo)
}
