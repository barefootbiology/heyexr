#' Get the segmentation data from the central scan
#'
#' Retrieves the segmentation data for the central b-scan, as specified in an
#' XML file.
#'
#' @param xml_file path to XML file containing Iowa Reference Algorithms segmentation
#' @param center_file path to XML file specifying the foveal center of the VOL data
#'
#' @return a data.frame (tbl_df) containing the central segmentation data
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter distinct group_by mutate ungroup
get_central_segmentation <- function(vol_file = NULL,
                                     xml_file = NULL,
                                     center_file = NULL,
                                     sample_id = NA) {

    # NOTE: The following lines are commented as the
    #       VOL files are now always available, for
    #       example, when an MDH file is segmented
    #       instead of a VOL file.
    # TASK: Wrap these functions to catch errors
    # Read the header
#     vol_con = file(vol_file, "rb")
#     header <- read_heyex_header(vol_con)
#     close(vol_con)

    # Read in the segmentation file
    oct_segmentation <- read_segmentation_xml(xml_file)

    # Read in the grid center file
    oct_center <- read_center_xml(center_file)


    # Get the grid center coordinates
    # NOTE: As far as I can tell, the coordinates used by OCT Explorere are always
    # 0-based, whereas (almost) everything in heyexr is 1-based, to fit in with
    # R. I should probably go through all the methods and some point and unify
    # them.
    center_x_voxel <- oct_center[["center"]][["x"]][[1]] + 1
    center_z_voxel <- oct_center[["center"]][["z"]][[1]] + 1

#     center_bscan <- oct$bscan_headers %>%
#         filter(bscan == center_y_voxel)

    # Adjust vertical scale to be 0 at RPE-level.
    # Convert the voxel numbers to be microns.
    result <- oct_segmentation$layers %>%
        filter(bscan_id == center_z_voxel) %>%
        group_by(ascan_id) %>%
        mutate(lowest_layer = surface_id == max(surface_id)) %>%
        mutate(base_value = max(value)) %>%
        mutate(value_adjusted = (-1 * (value - base_value)) *
                   oct_segmentation$info$'voxel_size_y') %>%
        mutate(x_adjusted = (ascan_id - center_x_voxel) *
                   oct_segmentation$info$'voxel_size_x') %>%
        ungroup() %>%
        mutate(sample_id = sample_id) # %>%
        # NOTE: The following lines are commented as the
        #       VOL files are not always available, for
        #       example, when an MDH file is segmented
        #       instead of a VOL file.
        # mutate(scan_position = header$scan_position)

    return(result)
}

