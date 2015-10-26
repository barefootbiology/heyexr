# TASK: Read B-Scans ------------------------
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
