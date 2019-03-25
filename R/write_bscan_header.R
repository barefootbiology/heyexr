#' Write b-scan header information to a file connection
#'
#' Writes the b-scan header information to a file connection.
#'
#' @param vol_con the connection to a VOL file
#' @param bscan_header_list previously imported b-scan header data
#'
#' @export
#' @importFrom magrittr %>%
write_bscan_header <- function(vol_con, bscan_header_list) {
    # For each B-Scan, read header and data
    # bscan_header_list <- list()

    # IMPLEMENT HEADER READING HERE.
    # CODE DIRECTLY FROM Open_Heyex_Raw.java
    # Read the first header.
    bscan_header_list$version %>%
        charToRaw() %>%
        pad_raw(n = 12) %>%
        writeBin(vol_con, "raw", endian = "little")
    writeBin(bscan_header_list$bscan_header_size, vol_con, integer(), endian = "little")
    writeBin(bscan_header_list$start_x, vol_con, double(), endian = "little")
    writeBin(bscan_header_list$start_y, vol_con, double(), endian = "little")
    writeBin(bscan_header_list$end_x, vol_con, double(), endian = "little")
    writeBin(bscan_header_list$end_y, vol_con, double(), endian = "little")
    writeBin(bscan_header_list$num_seg, vol_con, integer(), endian = "little")
    writeBin(bscan_header_list$off_seg, vol_con, integer(), endian = "little")
    writeBin(bscan_header_list$quality, vol_con, size = 4, endian = "little")
    # bscan_header_list$spare %>%
    #     charToRaw() %>%
    #     pad_raw(n = 196) %>%
    #     writeBin(vol_con, "raw")
    writeBin(raw(196), vol_con, "raw", endian = "little")

}
