#' Read b-scan header information from a connection to a VOL file
#'
#' Reads the b-scan header information from a connection to a VOL file. Function
#' based on Open_Heyex_Raw.java from the HEYEX plugin for ImageJ.
#'
#' @param vol_file the connection to a VOL file
#' @param header previously imported VOL file header data
#'
#' @return a list of the b-scan specfific header information
#'
#' @export
#' @importFrom magrittr %>%
read_bscan_header <- function(vol_file, header) {
    # For each B-Scan, read header and data
    bscan_header_list <- list()

    # IMPLEMENT HEADER READING HERE.
    # CODE DIRECTLY FROM Open_Heyex_Raw.java
    # Read the first header.
    bscan_header_list$version <- readBin(vol_file, "raw", size = 1, n = 12) %>% rawToChar()
    bscan_header_list$bscan_hdr_size <- readBin(vol_file, integer())
    bscan_header_list$start_x <- readBin(vol_file, double())
    bscan_header_list$start_y <- readBin(vol_file, double())
    bscan_header_list$end_x <- readBin(vol_file, double())
    bscan_header_list$end_y <- readBin(vol_file, double())
    bscan_header_list$num_seg <- readBin(vol_file, integer())
    bscan_header_list$off_seg <- readBin(vol_file, integer())
    bscan_header_list$quality <- readBin(vol_file, what="numeric", size = 4)
    bscan_header_list$spare <- readBin(vol_file, "raw", n = 196)

    return(bscan_header_list)
}
