#' Read a Heidelberg Spectralis VOL file
#'
#' Creates a list containing data from a Heidelberg Spectralis VOL file.
#'
#' @param vol_file path to VOL file
#' @param header_slo_only Import only the header information and SLO image?
#'
#' @return a list containing the data from the VOL file
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows mutate
read_vol_header_only <- function(vol_file) {
    # Code based on these two projects:
    #
    # https://github.com/halirutan/HeyexImport
    # http://rsb.info.nih.gov/ij/plugins/heyex/index.html

    # # Create a connection to the VOL file
    # vol_con = file(vol_file, "rb")

    # Read the entire file as a RAW vector, then
    # create a new connection in memory.
    vol_con <- readBin(vol_file, what = "raw", n = 2048) %>%
        rawConnection("rb")

    header <- read_vol_header(vol_con)

    # Close the connection to the VOL file
    close(vol_con)

    # # Return only the header and SLO image
    output <- list(header = header)

    # Return the requested object
    return(output)
}
