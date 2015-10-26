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

    cat("Offset to slo:",seek(vol_file, where = NA), "\n")
    # Read the SLO image
    slo_image <- read_heyex_slo(vol_file, header)

    # Read the first bscan_header
    cat("Before reading initial bscan_header:\t", seek(vol_file, where = NA), "\n")
    bscan_header_1 <- read_bscan_header(vol_file, header)
    cat("After reading initial bscan_header:\t", seek(vol_file, where = NA), "\n")

    close(vol_file)


    # Now, read in all the rest of the file as

    #segArraySize=numBscans*numSeg*sizeX;
    #segArray=new float[segArraySize];
    #segArrayByteSize=4*segArraySize;

    #bscan_header <- list()
    bscan_header_all <- list()

    seg_array = list()


    vol_file = file(x, "rb")

    # Read in the segmentation arrays
   for (bscan in c(0:(header$num_bscans-1))) {

#    for (bscan in 0) {
        #cat(bscan, "\n")

        #cat("Before seek bscan header:\t", seek(vol_file, where = NA), "\n")

#         seek(vol_file, where = bscan_header_1$file_header_size +
#                  bscan_header_1$slo_image_size +
#                  (bscan*bscan_header_1$bscan_block_size), origin = "start");
#
#         cat("After seek bscan header:\t", seek(vol_file, where = NA), "\n")

#         # Move the read position
#         bscan_header <- read_bscan_header(vol_file, header)



        seek(vol_file, where = bscan_header_1$file_header_size +
                 bscan_header_1$slo_image_size +
                 (bscan*bscan_header_1$bscan_block_size), origin = "start");

        cat("Before ", bscan+1, "bscan header:\t", seek(vol_file, where = NA), "\n")

        bscan_header_all[[bscan + 1]] <- read_bscan_header(vol_file, header)

        cat("After ", bscan+1, "bscan header:\t", seek(vol_file, where = NA), "\n")


        for (seg_layer in c(0:(bscan_header_1$num_seg-1))) {
            for (a_scan in c(0:(header$size_x-1))) {
                # R vectors and lists are indexed at 1
                index = 1 + a_scan + seg_layer*header$size_x + bscan*bscan_header_1$num_seg*header$size_x
                y_value <- readBin(vol_file, what = "raw",
                                   size = 1, n = 4,
                                   endian = "little") %>%
                    readBin(what = "numeric", size = 4, endian = "little")
                if ((y_value < 3.4028235E37) & !is.na(y_value)) {
                    seg_array[index] <- y_value
                } else {
                    seg_array[index] <- NA
                }
                    #rawConnection() %>%
                    # readBin(what = "numeric")
                #seg_array[index] <- readBin(vol_file, "double", size = 4, endian = "little")

                # TASK: Test if set to max float. If so, set as NA.
                #cat(bscan, "\t", seg_layer, "\t", a_scan, "\t", index, "\t", y_value,"\t", "end", "\n")

            }
        }
    }


    # Close the connection to the VOL file
    close(vol_file)

    # Return the requested object
    return(list(header = header,
                slo_image = slo_image,
                bscan_header=bscan_header_1,
                bscan_header_all = bscan_header_all,
                seg_array=seg_array))
}
