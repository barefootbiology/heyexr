#' Read OCTExplorer-ready NIFTI file
#'
#' Read OCTExplorer-ready NIFTI file in the manner of read_vol
#'
#' @export
#' @importFrom RNifti readNifti pixdim
#' @importFrom magrittr %>%
read_nifti <- function(nifti_file) {

    nifti <- readNifti(file = nifti_file)

    # The NIFTI file has been written to be compatible with OCTExplorer. We'll
    # have to reorder the dimensions in order to make it compatible with
    # octgridtools and heyexr:
    # x = ascan_id, y = bscan_id, z = depth

    reorder_dimensions <- c(1, 3, 2)

    voxel_size <- pixdim(nifti)[reorder_dimensions]

    bscan_images <-
        nifti %>%
        as.array() %>%
        aperm(reorder_dimensions)

    array_dim <- dim(bscan_images)

    header <- list()

    header$version      <- "ssoct"

    header$size_x       <- array_dim[1]
    header$num_bscans   <- array_dim[2]
    header$size_z       <- array_dim[3]

    header$scale_x      <- voxel_size[1]
    header$distance     <- voxel_size[2]
    header$scale_z      <- voxel_size[3]

    header$scan_position <- NA

    # To make the B-scan data compatible with the Heidelberg data,
    # flip the y-axis (B-scan ID).
    bscan_images <- bscan_images[, header$num_bscans:1, ]

    # TASK: Consider computing an SLO-like en face image and building the other
    #       components similar to those in the output from "read_vol".

    list(header = header,
         bscan_images = bscan_images)
}
