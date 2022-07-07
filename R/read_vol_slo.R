#' Read the VOL SLO image
#'
#' Read the SLO data from a Heidelberg Spectralis VOL file.
#'
#' @param con connection to a VOL file
#' @param header previously read header information
#'
#' @return a matrix containing the SLO image
#'
read_vol_slo <- function(vol_con, header) {
    # Originally, I built this function on the code from Open_Heyex_Raw.java
    # which was bundled with the heyex plugin for ImageJ. I have since verified
    # the code using the documentation for the Spectralis Special Function:
    # Exporting Raw Data document (revision 4.0-1E, Noveber 2008, Art. No.
    # 97 175-002).

    slo_data <- readBin(vol_con, integer(), size = 1,
                         n = header$size_x_slo * header$size_y_slo,
                         endian = "little", signed = FALSE)

    matrix(slo_data, nrow = header$size_x_slo)
}
