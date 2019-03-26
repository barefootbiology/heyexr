#' Read b-scan header information from a connection to a VOL file
#'
#' Reads the b-scan header information from a connection to a VOL file. Function
#' based on Open_Heyex_Raw.java from the HEYEX plugin for ImageJ.
#'
#' @param vol_con the connection to a VOL file
#'
#' @return a list of the b-scan specfific header information
#'
#' @importFrom magrittr %>%
read_bscan_header <- function(vol_con) {
    # For each B-Scan, read header and data
    bscan_header_list <- list()

    # IMPLEMENT HEADER READING HERE.
    # CODE DIRECTLY FROM Open_Heyex_Raw.java
    # Read the first header.
    bscan_header_list$version <-
        readBin(vol_con, "raw", size = 1, n = 12) %>%
        rawToChar()
    bscan_header_list$bscan_hdr_size <- readBin(vol_con, integer())
    bscan_header_list$start_x <- readBin(vol_con, double())
    bscan_header_list$start_y <- readBin(vol_con, double())
    bscan_header_list$end_x <- readBin(vol_con, double())
    bscan_header_list$end_y <- readBin(vol_con, double())
    bscan_header_list$num_seg <- readBin(vol_con, integer())
    bscan_header_list$off_seg <- readBin(vol_con, integer())
    bscan_header_list$quality <- readBin(vol_con, what="numeric", size = 4)
    bscan_header_list$spare <- list(bytes = readBin(vol_con, "raw", n = 196))

    bscan_header_list
}
