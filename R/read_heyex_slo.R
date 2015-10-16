# Read the header information from a Heidelberg Spectralis VOL file.
# Assumes offset of 2048 bytes.
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
