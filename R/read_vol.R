#' Read a Heidelberg Spectralis VOL file
#'
#' Creates a list containing data from a Heidelberg Spectralis VOL file.
#'
#' @param vol_file Path to VOL file.
#' @param read_what Character string indicating what parts of the file to read.
#'     If "all", then all data is read. If "slo", then only the header and SLO
#'     are read. Any other value will only return the header.
#' @param tz Timezone for dates in header.
#'
#' @return a list containing the data from the VOL file
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows mutate n
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
read_vol <- function(vol_file, read_what = "all", tz = "UTC") {
    # Code based on these two projects:
    #
    # https://github.com/halirutan/HeyexImport
    # http://rsb.info.nih.gov/ij/plugins/heyex/index.html

    if(!file.exists(vol_file)) {
        stop("File ", vol_file, " does not exist!")
    }

    vol_con <- file(vol_file, "rb")

    output <- list()

    header <- read_vol_header(vol_con, tz = tz)

    output$header <- header

    if(read_what %in% c("all", "slo")) {
        output$slo_image <- read_vol_slo(vol_con, header)
    }

    if(read_what == "all") {
        # Calculated offsets
        file_header_size = 2048; # integer, size in bytes of file header, equal to
                                 # offset to reach SLO image;
        slo_image_size = header$size_x_slo * header$size_y_slo; # integer, size in
                                        # bytes of the SLO image data;
        oct_image_size = header$size_x * header$size_z * 4; # integer, size in
                                        # bytes of each B-Scan OCT image;
        first_bscan_header = file_header_size + slo_image_size;
        bscan_block_size = header$bscan_hdr_size + oct_image_size;
        # integer, calculates size in bytes of the b-scan block which includes the
        # b-scan header and the b-scan;


        # Create empty containers
        bscan_header_all <- list()

        bscan_images <- array(rep(as.double(NA), header$size_x * header$num_bscans * header$size_z),
                             dim = c(header$size_x, header$num_bscans, header$size_z))

        # TASK: Update this comment
        # Up to 3 surfaces may be present. However, we won't know until we start
        # to read the b-scan headers below. Later we'll decrement the 3rd
        # dimension if there are only 2 segmentations present.
        seg_array <- array(rep(as.double(NA), header$num_bscans * 3 * header$size_x),
                           dim = c(header$size_x, header$num_bscans, 3))

        # Read in the segmentation arrays

        # pb <- txtProgressBar(min = 0,
        #                      max = header$num_bscans,
        #                      style = 1,
        #                      file = stderr())

        for (bscan_id in c(1:(header$num_bscans))) {
            # TESTING ************************************************
            cat("Value of bscan_id:\t", bscan_id, "\n")

            # setTxtProgressBar(pb, bscan_id, title = "Reading B-scans")

            bscan_header_all[[bscan_id]] <- read_bscan_header(vol_con)

            num_seg <- bscan_header_all[[bscan_id]]$num_seg

            if(num_seg > 3) {
                warning("WARNING: The header for B-scan ", bscan_id, " reports ", num_seg, " segmentations. To prevent problems in file reading, the number of segmentations is set to 3.")
                num_seg <- 3
            }

            # cat("\tbscan_header_all[[bscan_id]]:\n")
            # cat("\t", dput(as_tibble(bscan_header_all[[bscan_id]][1:9])), "\n")

            cat("\tValue of version:\t", bscan_header_all[[bscan_id]][["version"]], "\n")
            cat("\tValue of bscan_hdr_size:\t", bscan_header_all[[bscan_id]][["bscan_hdr_size"]], "\n")
            cat("\tValue of start_x:\t", bscan_header_all[[bscan_id]][["start_x"]], "\n")
            cat("\tValue of start_y:\t", bscan_header_all[[bscan_id]][["start_y"]], "\n")
            cat("\tValue of end_x:\t", bscan_header_all[[bscan_id]][["end_x"]], "\n")
            cat("\tValue of end_y:\t", bscan_header_all[[bscan_id]][["end_y"]], "\n")
            cat("\tValue of off_seg:\t", bscan_header_all[[bscan_id]][["off_seg"]], "\n")
            cat("\tValue of quality:\t", bscan_header_all[[bscan_id]][["quality"]], "\n")

            cat("\tValue of num_seg:\t", num_seg, "\n")


            seg_data <-
                read_float_vector(vol_con,
                                  n = header$size_x * num_seg) %>%
                matrix(nrow = num_seg,
                       ncol = header$size_x, byrow = FALSE)

            # If less than 3 segmentation surfaces are present, pad out the seg
            # data with NA's to be the correct size
            if(num_seg < 3) {
                seg_data <- c(seg_data, rep(as.numeric(NA),
                                            header$size_x * (3 - num_seg)))
            }



            cat("\tLength of seg_data:\t", length(seg_data), "\n")


            cat("\tDim of seg_array:\t", dim(seg_array), "\n")

            cat("\tDim of seg_array[ , bscan_id, ]:\t", dim(seg_array[ , bscan_id, ]), "\n")

            # TESTING ************************************************

            seg_array[ , bscan_id, ] <- seg_data

            n_bytes <-
                header$bscan_hdr_size - 256 -
                (num_seg * header$size_x * 4)

            # NOTE: Currently I'm not processing the extra data in these bytes,
            #       as they are simply for future file expansion in version
            #       HSF-OCT-101. However, I'm not sure if later versions do use
            #       these bytes.
            temp <- readBin(vol_con, "raw",
                            n = n_bytes)

            bscan_images[, bscan_id, ] <- readBin(vol_con,
                                              "numeric",
                                              n = header$size_x * header$size_z,
                                              size = 4) %>%
                matrix(nrow = header$size_z, ncol = header$size_x, byrow=FALSE)
        }

        # The text progressbar doesn't print a newline when complete.
        message("")

        # Make sure that NA values are properly represented in the seg_array
        bscan_images[bscan_images == max_float] <- NA

        seg_array[seg_array == max_float] <- NA

        # Add 1 to all distance value in seg_array to work with the 1-based
        # R indexing.
        seg_array <- seg_array + 1

        bscan_headers <-
            bscan_header_all %>%
            map_dfr(as_tibble) %>%
            # NOTE: Currently I'm throwing away the "spare" bytes, as these are
            #       not used in HSF-OCT-101. However, I'm not sure if they are
            #       used in later versions of the file format.
            select(-spare) %>%
            # For convenience, compute the coordinates in the SLO pixel space.
            # Adjust pixel values to match R's 1-based indexing system
            mutate(start_x_pixels = start_x / header$scale_x_slo + 1,
                   start_y_pixels = start_y / header$scale_y_slo + 1,
                   end_x_pixels = end_x / header$scale_x_slo + 1,
                   end_y_pixels = end_y / header$scale_y_slo + 1) %>%
            mutate(bscan_id = 1:n())

        output$bscan_headers <- bscan_headers
        output$seg_array <- seg_array
        output$bscan_images <- bscan_images
    }

    close(vol_con)

    output
}
