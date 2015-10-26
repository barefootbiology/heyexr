# Read a Heidelberg Spectralis VOL file.
read_heyex <- function(x) {
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

    # TASK: Read B-Scans ------------------------
    # For each B-Scan, read header and data
    bscan_header <- list()

    # IMPLEMENT HEADER READING HERE.
    # CODE DIRECTLY FROM Open_Heyex_Raw.java
    bscan_header$file_header_size = 2048; #integer, size in bytes of file heaader, equal to offset to reach SLO image;
    bscan_header$slo_image_size = header$scale_x_slo * header$scale_y_slo; # integer, size in bytes of the SLO image data;
    bscan_header$oct_image_size = header$size_x * header$size_y * 4; # integer, size in bytes of each B-Scan OCT image;
    bscan_header$first_bscan_header = bscan_header$file_header_size + bscan_header$slo_image_size;
    bscan_header$b_scan_block_size = header$b_scan_hdr_size + bscan_header$oct_image_size; # integer, calculates size in bytes of the b-scan block which includes the b-scan header and the b-scan;

    bscan_header$version <- readBin(vol_file, "raw", size =1, n = 12) %>% rawToChar()
    bscan_header$b_scan_hdr_size <- readBin(vol_file, integer())
    bscan_header$start_x <- readBin(vol_file, double())
    bscan_header$start_y <- readBin(vol_file, double())
    bscan_header$end_x <- readBin(vol_file, double())
    bscan_header$end_y <- readBin(vol_file, double())
    bscan_header$num_seg <- readBin(vol_file, integer())
    bscan_header$off_seg <- readBin(vol_file, integer())
    bscan_header$quality <- readBin(vol_file, what="numeric", size = 4)
    bscan_header$spare <- readBin(vol_file, "raw", n = 196)

    # Now, read in all the rest of the file as

    #segArraySize=numBscans*numSeg*sizeX;
    #segArray=new float[segArraySize];
    #segArrayByteSize=4*segArraySize;

    seg_array = list()

    for(b_scan in 0:(header$num_b_scans-1) ) {
        for(seg_layer in c(0:(bscan_header$num_seg-1))) {
            for (a_scan in c(0:(header$size_x))) {
                index = a_scan + seg_layer*header$size_x + b_scan*bscan_header$num_seg*header$size_x
                seg_array[index] <- readBin(vol_file, "numeric", size = 4)
            }
        }
    }

    # Close the connection to the VOL file
    close(vol_file)

    # Return the requested object
    return(list(header = header, slo_image = slo_image, bscan_header=bscan_header, seg_array=unlist(seg_array)))
}
