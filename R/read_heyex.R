library(bit64)
library(dplyr)

# Code based on these two projects:
#
# https://github.com/halirutan/HeyexImport
# http://rsb.info.nih.gov/ij/plugins/heyex/index.html


# Read the header information from a Heidelberg Spectralis VOL file.
# Assumes offset of 0 bytes.
read_heyex_header <- function(con) {
    # Create a container for the header information
    header <- list()

    # Read each value from the VOL file
    header$version      <- readBin(con, character(), endian = "little")
    header$size_x       <- readBin(con, integer(), endian = "little")
    header$num_b_scans  <- readBin(con, integer(), endian = "little")
    header$size_z       <- readBin(con, integer(), endian = "little")
    header$scale_x      <- readBin(con, double(), endian = "little")
    header$distance     <- readBin(con, double(), endian = "little")
    header$scale_z      <- readBin(con, double(), endian = "little")
    header$size_x_slo   <- readBin(con, integer(), endian = "little")
    header$size_y_slo   <- readBin(con, integer(), endian = "little")
    header$scale_x_slo  <- readBin(con, double(), endian = "little")
    header$scale_y_slo  <- readBin(con, double(), endian = "little")
    header$field_size_slo   <- readBin(con, integer(), endian = "little")
    header$scan_focus       <- readBin(con, double(), endian = "little")
    header$scan_position    <- readBin(con, character(), size = 1, n = 2, endian = "little")[1]

    # TASK: Convert to date/time
    # Convert exam time following "Open_Heyex_Info.java"
    header$exam_time        <- readBin(con, "raw", endian = "little",  n = 8,
                                       signed = FALSE) %>%
        readBin(integer64())

    header$scan_pattern     <- readBin(con, integer(), endian = "little")
    header$b_scan_hdr_size  <- readBin(con, integer(), endian = "little")

    header$id               <- readBin(con, "raw", endian = "little", size = 1, n = 16) %>%
        rawToChar()
    header$reference_id     <- readBin(con, "raw", endian = "little", size = 1, n = 16) %>%
        rawToChar()
    header$pid              <- readBin(con, integer(), endian = "little")
    header$patient_id       <- readBin(con, "raw", endian = "little", size = 1, n = 21) %>%
        rawToChar() # , size = 21
    header$padding          <- readBin(con, "raw", endian = "little", n = 3)

    # TASK: Convert to date/time
    # Convert DOB following "Open_Heyex_Info.java"
    header$dob              <- readBin(con, double(), endian = "little", size = 8)

    header$vid              <- readBin(con, integer(), endian = "little")
    header$visit_id         <- readBin(con, "raw", endian = "little", size = 1, n = 24) %>%
        rawToChar()

    # TASK: Convert to date/time
    # Convert DOB following "Open_Heyex_Info.java"
    header$visit_date       <- readBin(con, double(), endian = "little")

    header$spare            <- readBin(con, "raw", endian = "little", size = 1, n = 1840)

    # Return the header. The file connection is automatically updated.
    return(header = header)
}

# Read the header information from a Heidelberg Spectralis VOL file.
# Assumes offset of 2048 bytes.
read_heyex_slo <- function(con, header) {
    slo_image <- readBin(con, integer(), size = 1,
            n = header$size_x_slo * header$size_y_slo,
            endian = "little", signed = FALSE) %>%
        (function(x) matrix(x, nrow = header$size_x_slo))

    return(slo_image)
}

# Read a Heidelberg Spectralis VOL file.
read_heyex <- function(x) {
    # Create a connection to the VOL file
    vol_file = file(x, "rb")

    # Read the header
    header <- read_heyex_header(vol_file)

    # Read the SLO image
    slo_image <- read_heyex_slo(vol_file, header)

#          $bScanHeaderInfo = Transpose[{
#              {
#                  "Version", "BScanHdrSize", "StartX", "StartY", "EndX", "EndY" ,
#                  "NumSeg", "OffSeg", "Quality", "Spare"
#              },
#              {{b, 12}, i, d, d, d, d, i, i, f, {b, 196}}
#          }];
#          ];


    # TASK: Read B-Scans
    # For each B-Scan, read header and data


    # Close the connection to the VOL file
    close(vol_file)

    # Return the requested object
    return(list(header = header, slo_image = slo_image))
    # return(header)
}
