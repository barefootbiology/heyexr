#' Read the VOL SLO image
#'
#' Read the header information from a Heidelberg Spectralis VOL file.
#' Function assumes the header begins at byte 2048.
#'
#' @param con connection to a VOL file
#' @param header previously read header information
#'
#' @return a matrix containing the SLO image
#'
#' @export
#' @importFrom magrittr %>%
read_heyex_slo <- function(con, header) {
    # Code based on these two projects:
    #
    # https://github.com/halirutan/HeyexImport
    # http://rsb.info.nih.gov/ij/plugins/heyex/index.html

    slo_image <- readBin(con, integer(), size = 1,
                         n = header$size_x_slo * header$size_y_slo,
                         endian = "little", signed = FALSE) %>%
        (function(x) matrix(x, nrow = header$size_x_slo))

    return(slo_image)
}
