#' Write a VOL header to a file connection.
#'
#' \code{write_vol_header} writes the data in a VOL object to a file connection.
#'
#' @param vol_con File connection object.
#' @param header Header (list) from a VOL object.
#'
#' @importFrom magrittr %>%
write_vol_header <- function(vol_con, header) {

    writeBin(header$version, vol_con, character(), endian = "little")

    writeBin(header$size_x, vol_con, integer(), endian = "little")

    writeBin(header$num_bscans, vol_con, integer(), endian = "little")

    writeBin(header$size_z, vol_con, integer(), endian = "little")

    writeBin(header$scale_x, vol_con, double(), endian = "little")

    writeBin(header$distance, vol_con, double(), endian = "little")

    writeBin(header$scale_z, vol_con, double(), endian = "little")

    writeBin(header$size_x_slo, vol_con, integer(), endian = "little")

    writeBin(header$size_y_slo, vol_con, integer(), endian = "little")

    writeBin(header$scale_x_slo, vol_con, double(), endian = "little")

    writeBin(header$scale_y_slo, vol_con, double(), endian = "little")

    writeBin(header$field_size_slo, vol_con, integer(), endian = "little")

    writeBin(header$scan_focus, vol_con, double(), endian = "little")

    charToRaw(header$scan_position) %>%
        pad_raw(size = 1, n = 4) %>%
        writeBin(vol_con, "raw", endian = "little")

    datetime_to_raw(header$exam_time) %>%
        writeBin(vol_con, "raw", endian = "little")

    # From: https://stat.ethz.ch/R-manual/R-devel/library/base/html/DateTimeClasses.html
    # "Class "POSIXct" represents the (signed) number of seconds
    # since the beginning of 1970 (in the UTC time zone) as a numeric vector."
    # That means that R's reference is the same as Java's.

    writeBin(header$scan_pattern, vol_con, integer(), endian = "little")

    writeBin(header$bscan_hdr_size, vol_con, integer(), endian = "little")

    charToRaw(header$id) %>%
        pad_raw(size = 1, n = 16) %>%
        writeBin(vol_con, "raw", endian = "little")

    charToRaw(header$reference_id) %>%
        pad_raw(size = 1, n = 16) %>%
        writeBin(vol_con, "raw", endian = "little")

    writeBin(header$pid, vol_con, integer(), endian = "little")

    charToRaw(header$patient_id) %>%
        pad_raw(size = 1, n = 21) %>%
        writeBin(vol_con, "raw", endian = "little")

    raw(3) %>%
        writeBin(vol_con, "raw", endian = "little")

    day_offset       <- 25569

    dob <- as.numeric(header$dob) / (60 * 60 * 24) + day_offset
    writeBin(dob, vol_con, double(), endian = "little")

    writeBin(header$vid, vol_con, integer(), endian = "little")

    charToRaw(header$visit_id) %>%
        pad_raw(size = 1, n = 24) %>%
        writeBin(vol_con, "raw", endian = "little")

    visit_date <- as.numeric(header$visit_date)  / (60 * 60 * 24) + day_offset
    writeBin(visit_date, vol_con, double(), endian = "little")

    # TASK: Update this to write out the original spare data
    raw(1840) %>%
        writeBin(vol_con, "raw", endian = "little")

}
