#' Read the VOL header
#'
#' Read the header information from a Heidelberg Spectralis VOL file.
#' Function assumes the header begins at byte 0. Due to limitation in R,
#' this function does not currently parse date and time information correctly.
#'
#' @param vol_con a connection to the VOL file
#'
#' @return a list containing the header from the VOL file
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
read_vol_header <- function(vol_con) {
    # Code based on these two projects:
    #
    # https://github.com/halirutan/HeyexImport
    # http://rsb.info.nih.gov/ij/plugins/heyex/index.html

    # Create a container for the header information
    header <- list()

    # Read each value from the VOL file
    header$version      <- readBin(vol_con, character(), endian = "little")
    header$size_x       <- readBin(vol_con, integer(), endian = "little")
    header$num_bscans   <- readBin(vol_con, integer(), endian = "little")
    header$size_z       <- readBin(vol_con, integer(), endian = "little")
    header$scale_x      <- readBin(vol_con, double(), endian = "little")
    header$distance     <- readBin(vol_con, double(), endian = "little")
    header$scale_z      <- readBin(vol_con, double(), endian = "little")
    header$size_x_slo   <- readBin(vol_con, integer(), endian = "little")
    header$size_y_slo   <- readBin(vol_con, integer(), endian = "little")
    header$scale_x_slo  <- readBin(vol_con, double(), endian = "little")
    header$scale_y_slo  <- readBin(vol_con, double(), endian = "little")
    header$field_size_slo   <- readBin(vol_con, integer(), endian = "little")
    header$scan_focus       <- readBin(vol_con, double(), endian = "little")
    header$scan_position    <- readBin(vol_con, character(), size = 1, n = 2, endian = "little")[1]

    # TASK: Convert to date/time
    # Convert exam time following "Open_Heyex_Info.java"
    header$exam_time        <- readBin(vol_con, "raw", endian = "little",  n = 8,
                                       signed = FALSE) %>%
        raw_to_datetime()
    # From: https://stat.ethz.ch/R-manual/R-devel/library/base/html/DateTimeClasses.html
    # "Class "POSIXct" represents the (signed) number of seconds
    # since the beginning of 1970 (in the UTC time zone) as a numeric vector."
    # That means that R's reference is the same as Java's.

    header$scan_pattern     <- readBin(vol_con, integer(), endian = "little")
    header$bscan_hdr_size  <- readBin(vol_con, integer(), endian = "little")

    header$id               <- readBin(vol_con, "raw", endian = "little",
                                       size = 1, n = 16) %>%
        rawToChar()

    header$reference_id     <- readBin(vol_con, "raw", endian = "little",
                                       size = 1, n = 16) %>%
        rawToChar()

    header$pid              <- readBin(vol_con, integer(), endian = "little")
    header$patient_id       <- readBin(vol_con, "raw", endian = "little",
                                       size = 1, n = 21) %>%
        rawToChar() # , size = 21

    header$padding          <- readBin(vol_con, "raw", endian = "little",
                                       n = 3) %>%
        paste0(collapse = "")

    # TASK: Convert to date/time
    # Convert DOB following "Open_Heyex_Info.java"
    # day_offset <- difftime(ymd("1970-01-01"), ymd("1899-12-30"), units = "days") %>%
    #     as.double()
    day_offset       <- 25569
    dob              <- readBin(vol_con, double(), endian = "little", size = 8)

    header$dob      <- ((floor(dob) - day_offset)) %>%
        as.Date(origin = as.POSIXct("1970-01-01", tz="UTC"))

    header$vid      <- readBin(vol_con, integer(), endian = "little")
    header$visit_id     <- readBin(vol_con, "raw", endian = "little", size = 1, n = 24) %>%
        rawToChar()


    # TASK: Convert to date/time
    # Convert DOB following "Open_Heyex_Info.java"
    visit_date      <- readBin(vol_con, double(), endian = "little")

    # NOTE: I think this works. It makes sense at least.
    header$visit_date <- ((floor(visit_date) - day_offset)) %>%
        as.Date(origin = as.POSIXct("1970-01-01", tz="UTC"))

    # NOTE: We're discarding this data, as Heidelberg isn't storing anything of
    #       interest in this.
    spare <- readBin(vol_con, "raw", endian = "little", size = 1, n = 1840)

    # Return the header. The file connection is automatically updated.
    return(header = header)
}
