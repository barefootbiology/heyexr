# TASK: Read B-Scans ------------------------
read_bscan_header <- function(vol_file, header) {
    # For each B-Scan, read header and data
    bscan_header <- list()

    # IMPLEMENT HEADER READING HERE.
    # CODE DIRECTLY FROM Open_Heyex_Raw.java
    # Read the first header.

    bscan_header$file_header_size = 2048; #integer, size in bytes of file heaader, equal to offset to reach SLO image;
    bscan_header$slo_image_size = header$scale_x_slo * header$scale_y_slo; # integer, size in bytes of the SLO image data;
    bscan_header$oct_image_size = header$size_x * header$size_y * 4; # integer, size in bytes of each B-Scan OCT image;
    bscan_header$first_bscan_header = bscan_header$file_header_size + bscan_header$slo_image_size;
    bscan_header$b_scan_block_size = header$b_scan_hdr_size + bscan_header$oct_image_size; # integer, calculates size in bytes of the b-scan block which includes the b-scan header and the b-scan;

    bscan_header$version <- readBin(vol_file, "raw", size = 1, n = 12) %>% rawToChar()
    bscan_header$b_scan_hdr_size <- readBin(vol_file, integer())
    bscan_header$start_x <- readBin(vol_file, double())
    bscan_header$start_y <- readBin(vol_file, double())
    bscan_header$end_x <- readBin(vol_file, double())
    bscan_header$end_y <- readBin(vol_file, double())
    bscan_header$num_seg <- readBin(vol_file, integer())
    bscan_header$off_seg <- readBin(vol_file, integer())
    bscan_header$quality <- readBin(vol_file, what="numeric", size = 4)
    bscan_header$spare <- readBin(vol_file, "raw", n = 196)

    return(bscan_header)
}

# Read a Heidelberg Spectralis VOL file.
read_heyex2 <- function(x) {
    # Code based on these two projects:
    #
    # https://github.com/halirutan/HeyexImport
    # http://rsb.info.nih.gov/ij/plugins/heyex/index.html

    # Create a connection to the VOL file
    vol_file = file(x, "rb")

    # Read the header
    header <- read_heyex_header(vol_file)

    # Read the SLO image
    slo_image <- read_heyex_slo(vol_file, header)

    # Read the first b_scan_header
    bscan_header <- read_bscan_header(vol_file, header)

    # Now, read in all the rest of the file as

    #segArraySize=numBscans*numSeg*sizeX;
    #segArray=new float[segArraySize];
    #segArrayByteSize=4*segArraySize;

    seg_array = list()

    close(vol_file)

    vol_file = file(x, "rb")

    # Read in the segmentation arrays
#    for (b_scan in c(0:(header$num_b_scans-1))) {
    for (b_scan in c(0:(header$num_b_scans-1))) {
        #cat(b_scan, "\n")

        # Move the read position
        seek(vol_file, where = bscan_header$file_header_size +
                 bscan_header$slo_image_size +
                 (b_scan*bscan_header$b_scan_block_size) +
                 256, origin = "start");

        for (seg_layer in c(0:(bscan_header$num_seg-1))) {
            for (a_scan in c(0:(header$size_x-1))) {
                # R vectors and lists are indexed at 1
                index = 1 + a_scan + seg_layer*header$size_x + b_scan*bscan_header$num_seg*header$size_x
                y_value <- readBin(vol_file, what = "raw",
                                   size = 1, n = 4,
                                   endian = "little") %>%
                    readBin(what = "numeric", size = 4, endian = "little")
                if ((y_value != 3.4028235E38) | is.na(y_value)) {
                    seg_array[index] <- y_value
                } else {
                    seg_array[index] <- NA
                }
                    #rawConnection() %>%
                    # readBin(what = "numeric")
                #seg_array[index] <- readBin(vol_file, "double", size = 4, endian = "little")

                # TASK: Test if set to max float. If so, set as NA.
                cat(b_scan, "\t", seg_layer, "\t", a_scan, "\t", index, "\t", y_value,"\t", "end", "\n")

            }
        }
    }

    # seg_array = NULL

    # Now start reading the file from the beginning
    # I can probably speed this up at some point. See Open_Heyex_Raw.java for details.
#     for(b_scan in 1:(header$num_b_scans) ) {
#         seek(vol_file, where = bscan_header$file_header_size + bscan_header$slo_image_size + (b_scan-1)*bscan_header$b_scan_block_size + 256, origin = "start");
#
#         seg_array[b_scan] <- readBin(vol_file, "numeric", size = 4, n = header$num_b_scans * bscan_header$num_seg * header$size_x)
#
# #         for(seg_layer in c(0:(bscan_header$num_seg-1))) {
#             for (a_scan in c(0:(header$size_x))) {
#                 index = a_scan + seg_layer*header$size_x + b_scan*bscan_header$num_seg*header$size_x
#                 seg_array[index] <- readBin(vol_file, "numeric", size = 4)
#             }
#         }
#    }


    # Close the connection to the VOL file
    close(vol_file)

    # Return the requested object
    return(list(header = header, slo_image = slo_image, bscan_header=bscan_header, seg_array=seg_array))
}
