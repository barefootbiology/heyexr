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
read_heyex_noloop <- function(vol_file, header_slo_only = FALSE) {
    # Code based on these two projects:
    #
    # https://github.com/halirutan/HeyexImport
    # http://rsb.info.nih.gov/ij/plugins/heyex/index.html

    # Create a connection to the VOL file
    vol_con = file(vol_file, "rb")

    # Read the header
    header <- read_heyex_header(vol_con)

    # cat("Offset to slo:",seek(vol_con, where = NA), "\n")
    # Read the SLO image
    slo_image <- read_heyex_slo(vol_con, header)

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
        seg_array <- list()
        bscan_image <- list()

        # TASK: Move this to an external function.
        # Read in the segmentation arrays

        pb <- txtProgressBar(min = 0,
                             max = header$num_bscans,
                             style = 1,
                             file = stderr())

        for (bscan in c(0:(header$num_bscans-1))) {
            setTxtProgressBar(pb, bscan+1, title = "Reading B-Scans")

            bscan_header_all[[bscan + 1]] <- read_bscan_header(vol_con, header)

            # Read in the Heidelberg segmentation information
            for (seg_layer in c(0:(bscan_header_all[[bscan+1]]$num_seg-1))) {
                for (a_scan in c(0:(header$size_x-1))) {
                    # R vectors and lists are indexed at 1
                    index = 1 +
                        a_scan +
                        seg_layer*header$size_x +
                        bscan*bscan_header_all[[bscan+1]]$num_seg*header$size_x

                    y_value <- read_float(vol_con)
                    if ((y_value < 3.4028235E37) & !is.na(y_value)) {
                        seg_array[index] <- y_value
                    } else {
                        seg_array[index] <- NA
                    }
                }
            }

            temp <- readBin(vol_con, "raw", n = (header$bscan_hdr_size - 256 - (bscan_header_all[[bscan+1]]$num_seg*header$size_x*4)))

            bscan_image[[length(bscan_image) + 1]] <- read_float_vector(vol_con, n = header$size_x * header$size_z)

        }

        # TASK: Convert bscan_headers to a data.frame.
        #       We can get rid of the "spare" column.
        bscan_header_all <- lapply(bscan_header_all, function(x) x[1:9] %>%
                                         unlist %>%
                                         matrix(nrow = 1, byrow = FALSE) %>%
                                         data.frame(stringsAsFactors = FALSE)) %>%
            bind_rows %>%
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
            mutate(start_x_pixels = start_x / header$scale_x_slo,
                   start_y_pixels = start_y / header$scale_y_slo,
                   end_x_pixels = end_x / header$scale_x_slo,
                   end_y_pixels = end_y / header$scale_y_slo) %>%
            mutate(bscan = 1:n())

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
