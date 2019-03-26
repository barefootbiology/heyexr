#' Write the data in a volume object to a VOL file.
#'
#' \code{write_vol} writes a volume object to a VOL file.
#' TASK: DESCRIBE DIFFERENCES BETWEEN THE HSF-OCT-101 FORMAT AND WHAT THIS
#' FUNCTION PRODUCES.
#'
#' @param volume A volume object.
#' @param vol_file The target file.
#' @param overwrite If the target exists, should it be overwritten?
#'
#' @export
#' @importFrom magrittr %>%
write_vol <- function(volume, vol_file, overwrite = FALSE) {
    if(!overwrite & file.exists(vol_file)) {
        stop("File ", vol_file, " exists!")
    }

    vol_con <- file(vol_file, "wb")

    header <- volume$header

    write_vol_header(vol_con = vol_con, header = header)
    write_vol_slo(vol_con = vol_con, slo = volume$slo_image)

    ## WRITE B-SCAN DATA
    for (bscan_id in c(1:(header$num_bscans))) {

        # Write bscan header (remembering to adjust for 0-based coordinates)
        bscan_header_list <- volume$bscan_headers[bscan_id,]

        write_bscan_header(vol_con, bscan_header_list)

        # Write segmentation array (Only writing the correct number of surfaces present)
        # (remembering to replace NAs with the max_float value)
        seg_array <- c(volume$seg_array[,bscan_id, 1:bscan_header_list$num_seg])

        seg_array[is.na(seg_array)] <- max_float

        # Return to 0-based coordinates
        seg_array <- seg_array - 1

        writeBin(seg_array, vol_con, size = 4, endian = "little")

        # Write fill bytes
        n_bytes <- header$bscan_hdr_size - 256 - bscan_header_list$num_seg * header$size_x * 4

        rep(max_float, times = n_bytes / 4) %>%
            writeBin(vol_con, size = 4, endian = "little")

        # Write bscan image
        # (remembering to replace NAs with the max_float value)
        bscan_image <- c(volume$bscan_images[, bscan_id, ])
        bscan_image[is.na(bscan_image)] <- max_float

        writeBin(bscan_image, vol_con, size = 4, endian = "little")
    }
    close(vol_con)
}