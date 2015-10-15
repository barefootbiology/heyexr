read_heyex_header <- function(con) {
    # Create a container for the header information
    header <- list()

    # Read each value from the VOL file
    header$version      <- readBin(con, character(), size = 12, endian = "little")
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
    header$scan_position    <- readBin(con, character(), size = 4, endian = "little")
    header$exam_time1        <- readBin(con, integer(), n = 2, endian = "little")
    # header$exam_time2        <- readBin(con, integer(),  endian = "little")
    header$scan_pattern     <- readBin(con, integer(), endian = "little")
    header$b_scan_hdr_size  <- readBin(con, integer(), endian = "little")
    header$id               <- readBin(con, character(), size = 16, endian = "little")
    header$reference_id     <- readBin(con, character(), size = 16, endian = "little")
    header$pid              <- readBin(con, integer(), endian = "little")
    header$patient_id       <- readBin(con, character(), size = 21, endian = "little")
    header$padding          <- readBin(con, character(), size = 3, endian = "little")
    header$dob              <- readBin(con, double(), endian = "little")
    header$vid              <- readBin(con, integer(), endian = "little")
    header$visit_id         <- readBin(con, character(), size = 24, endian = "little")
    header$visit_date       <- readBin(con, double(), endian = "little")
    header$spare            <- readBin(con, character(), size = 1840, endian = "little")

    # Return both the header information and the file connection
    return(list(header = header, con = con))
}

read_slo_image <- function(con, header) {
    #readSLOImage[str_InputStream, fileHdr : {(_String -> _) ..}] :=
    #    Image[Partition[
    #        BinaryReadList[str, "Byte" , "SizeXSlo" * "SizeYSlo" /. fileHdr],
    #        "SizeXSlo" /. fileHdr], "Byte" ];
    # Read in as a vector
    slo_image <- readBin(con, character(), size = header$size_x_slo * header$size_y_slo, endian = "little")

    # TASK: Convert to a matrix

    return(list(slo_image = slo_image, con = con))
}


read_heyex <- function(x) {
    # Create a connection to the VOL file
    vol_file = file(x, "rb")

    # Read the header
    results <- read_heyex_header(vol_file)
    header <- results$header

    # Read the SLO image

    #results <- read_slo_image(results$con, header)
    #slo_image <- results$slo_image
    #vol_files <- results$con

    slo_image <- matrix(readBin(results$con, character(),
                         n = header$size_x_slo * header$size_y_slo,
                         endian = "little"), nrow = header$size_x_slo)


#     With[{i = "Integer32", f = "Real32", d = "Real64", b = "Byte"},
#
#          $fileHeaderInfo = Transpose[{
#              {
#                  "Version", "SizeX", "NumBScans", "SizeZ", "ScaleX", "Distance" ,
#                  "ScaleZ", "SizeXSlo", "SizeYSlo", "ScaleXSlo", "ScaleYSlo" ,
#                  "FieldSizeSlo", "ScanFocus", "ScanPosition", "ExamTime" ,
#                  "ScanPattern", "BScanHdrSize", "ID", "ReferenceID", "PID" ,
#                  "PatientID", "Padding", "DOB", "VID", "VisitID", "VisitDate" ,
#                  "Spare"
#              },
#              {
#                  {b, 12}, i, i, i, d, d, d, i, i, d, d, i, d, {b, 4}, {i, 2}, i, i,
#                  {b, 16}, {b, 16}, i, {b, 21}, {b, 3}, d, i, {b, 24}, d, {b, 1840}
#              }
#          }];
#
#          $bScanHeaderInfo = Transpose[{
#              {
#                  "Version", "BScanHdrSize", "StartX", "StartY", "EndX", "EndY" ,
#                  "NumSeg", "OffSeg", "Quality", "Spare"
#              },
#              {{b, 12}, i, d, d, d, d, i, i, f, {b, 196}}
#          }];
#          ];

    # Read SLO Image


    # Read B-Scans


    # Close the connection to the VOL file
    close(vol_file)

    # Return the requested object
    return(list(header = header, slo_image = slo_image))
}
