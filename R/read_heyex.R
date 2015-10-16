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




    # Close the connection to the VOL file
    close(vol_file)

    # Return the requested object
    return(list(header = header, slo_image = slo_image))
}
