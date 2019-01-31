#' Read a Heidelberg Spectralis VOL file
#'
#' Creates a list containing data from a Heidelberg Spectralis VOL file.
#'
#' @param vol_file path to VOL file
#' @param header_slo_only Import only the header information and SLO image?
#'
#' @return a list containing the data from the VOL file
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows mutate
read_vol <- function(vol_file, header_slo_only = FALSE) {
    # Code based on these two projects:
    #
    # https://github.com/halirutan/HeyexImport
    # http://rsb.info.nih.gov/ij/plugins/heyex/index.html

    # Create a connection to the VOL file
    vol_con = file(vol_file, "rb")

    # Read the header
    header <- read_vol_header(vol_con)

    # cat("Offset to slo:",seek(vol_con, where = NA), "\n")
    # Read the SLO image
    slo_image <- read_vol_slo(vol_con, header)

    if(!header_slo_only) {
        # Calculated offests
        file_header_size = 2048; #integer, size in bytes of file header, equal to
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
        # seg_array <- list()
        # bscan_image <- list()

        bscan_image <- array(rep(as.double(NA), header$size_x * header$num_bscans * header$size_z),
                             dim = c(header$size_x, header$num_bscans, header$size_z))

        # TASK: Update this comment
        # Up to 3 surfaces may be present. However, we won't know until we start
        # to read the b-scan headers below. Later we'll decrement the 3rd
        # dimension if there are only 2 segmentations present.
        seg_array <- array(rep(as.double(NA), header$num_bscans * 3 * header$size_x),
                           dim = c(header$size_x, header$num_bscans, 3))

        # TASK: Move this to an external function.
        # Read in the segmentation arrays

        pb <- txtProgressBar(min = 0,
                             max = header$num_bscans,
                             style = 1,
                             file = stderr())

        for (bscan_id in c(1:(header$num_bscans))) {
            setTxtProgressBar(pb, bscan_id, title = "Reading B-scans")

            bscan_header_all[[bscan_id]] <- read_bscan_header(vol_con, header)

            num_seg <- bscan_header_all[[bscan_id]]$num_seg
            # if(bscan == 1) {
            #     if(bscan_header_all[[bscan]]$num_seg != dim(seg_array)[3]) {
            #         seg_array <- seg_array[ , , 1:bscan_header_all[[bscan]]$num_seg]
            #     }
            # }

            # # TESTING:
            # message("bscan:\t", bscan)
            # message("header$bscan_hdr_size:\t", header$bscan_hdr_size)
            # message("bscan_header_all[[bscan]]$end_y:\t", bscan_header_all[[bscan]]$end_y)
            # message("bscan_header_all[[bscan]]$num_seg:\t", num_seg)
            # message("header$size_x:\t",header$size_x)

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

            seg_array[ , bscan_id, ] <- seg_data

            # NOTE: I think this is simply to move the file buffer along. Not
            #       sure if I should substitute "readBin" with "seek".


            n_bytes <- header$bscan_hdr_size - 256 - (num_seg * header$size_x * 4)
            # message("n_bytes\t", n_bytes)

            temp <- readBin(vol_con, "raw",
                            n = n_bytes)

            bscan_image[, bscan_id, ] <- readBin(vol_con,
                                              "numeric",
                                              n = header$size_x * header$size_z,
                                              size = 4) %>%
                matrix(nrow = header$size_z, ncol = header$size_x, byrow=FALSE)
        }

        # The text progressbar doesn't print a newline when complete.
        message("")

        # Make sure that NA values are properly represented in the seg_array
        bscan_image[bscan_image == max_float] <- NA

        seg_array[seg_array == max_float] <- NA

        # Add 1 to all distance value in seg_array to work with the 1-based
        # R indexing
        seg_array <- seg_array + 1

        # TASK: Convert bscan_headers to a data.frame.
        #       We can get rid of the "spare" column.
        bscan_header_all <- lapply(bscan_header_all, function(x) x[1:9] %>%
                                         unlist %>%
                                         matrix(nrow = 1, byrow = FALSE) %>%
                                         data.frame(stringsAsFactors = FALSE)) %>%
            bind_rows() %>%
            setNames(c("version", "bscan_header_size",
                       "start_x", "start_y",
                       "end_x", "end_y", "num_seg",
                       "off_seg", "quality")) %>%
            mutate(bscan_header_size = as.numeric(bscan_header_size),
                   start_x = as.numeric(start_x),
                   start_y = as.numeric(start_y),
                   end_x = as.numeric(end_x),
                   end_y = as.numeric(end_y),
                   num_seg = as.numeric(num_seg),
                   off_seg = as.numeric(off_seg),
                   quality = as.numeric(quality)) %>%
            # For convenience, compute the coordinates in the SLO pixel space.
            # Adjust pixel values to match R's 1-based indexing system
            mutate(start_x_pixels = start_x / header$scale_x_slo + 1,
                   start_y_pixels = start_y / header$scale_y_slo + 1,
                   end_x_pixels = end_x / header$scale_x_slo + 1,
                   end_y_pixels = end_y / header$scale_y_slo + 1) %>%
            mutate(bscan_id = 1:n())

        output <- list(header = header,
                       slo_image = slo_image,
                       bscan_headers = bscan_header_all,
                       seg_array = seg_array,
                       bscan_images = bscan_image)

    } else {
        # Return only the header and SLO image
        output <- list(header = header,
                       slo_image = slo_image)
    }

    # Close the connection to the VOL file
    close(vol_con)

    # Return the requested object
    return(output)
}
