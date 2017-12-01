#' Render summary plots of SLO and b-scans for an OCT VOL file
#'
#' Generates plots from a VOL file. Segmentation from the Iowa Reference
#' Algorithms can be included as an optional overlay on the b-scans.
#'
#' @param vol_file path to VOL file
#' @param xml_file path to XML file containing Iowa Reference Algorithms segmentation
#' @param out_dir name of directory to save rendered results
#' @param return_results Return the data read as a list?
#'
#' @return a list containing the OCT and segmentation data (if requested)
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter collect distinct select
#' @importFrom ggplot2 scale_color_brewer element_text theme ggsave scale_color_manual geom_line geom_segment geom_tile geom_vline aes ggplot annotate
#' @importFrom gridExtra arrangeGrob
#' @importFrom parallel parLapply makeCluster clusterCall stopCluster
#' @importFrom purrr walk
#' @importFrom ggmap theme_nothing
render_oct_summary <- function(vol_file,
                               xml_file = NULL,
                               center_file = NULL,
                               out_dir = "rendered_bscans",
                               return_results=FALSE,
                               n_cores = 1,
                               prefix = "",
                               overlay_bscan_position = TRUE,
                               omit_slo = FALSE,
                               overlay_heidelberg_segmentation = TRUE,
                               crop_to_heidelberg_segmentation = c(10, 75),
                               file_type = "pdf") {

    # TASK: Ensure that the render function will work with or without
    #       segmentation from OCT explorer.

    base_name <- basename(vol_file) %>%
        gsub(pattern=".VOL", replacement="", ignore.case = TRUE)

    # From the results directory, construct the other file paths
    output_path <- file.path(out_dir)

    if(!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE)
    }

    # Load data ------------------------------------------
    oct <- NULL
    oct_segmentation <- NULL
    oct_center <- NULL

    # TASK: Decided if the intermediate files should be saved
    # Load raw data from VOL or RData file
    if(!file.exists(paste(vol_file, ".RData", sep=""))) {
        # Load the VOL data
        oct <- read_heyex(vol_file)
        save(oct, file = paste(vol_file, ".RData", sep=""))
    } else {
        load(paste(vol_file, ".RData", sep=""))
    }

    # If an XML file is provided,
    # load segmentation from XML or RData file.
    if(!is.null(xml_file)) {
        if(!file.exists(paste(xml_file, ".RData", sep=""))) {
            # Load the segmentation from OCT Explorer
            oct_segmentation <- read_segmentation_xml(xml_file)
            save(oct_segmentation, file = paste(xml_file, ".RData", sep=""))
        } else {
            load(paste(xml_file, ".RData", sep=""))
        }
    }

    # TASK: Load the "grid center" XML file
    if(!is.null(center_file)) {
        oct_center <- read_center_xml(center_file)
    }

    # TASK: Once the data has been returned to the user, I should probably
    #       check to see if the data in meta data from the VOL file, the
    #       segmentation file, and the grid center file all match. If not,
    #       the function should at least return a warning.


    # Build report elements ------------------------------------------
    p_slo <- construct_slo(oct, draw_margins = TRUE)

    # Get the Heidelberg segmentation
    oct_seg_array <- try(get_segmentation(oct))

    # Function from http://adv-r.had.co.nz/Exceptions-Debugging.html
    is.error <- function(x) inherits(x, "try-error")


    # The order of b-scans and segmentation results are opposite within the
    # VOL and XML files. Thus, we build this handy mapping structure to
    # assign the correct segmentation results to the correct B-scans.
    b_n_seg <- 1:oct$header$num_bscans %>%
        setNames(oct$header$num_bscans:1)

    # Theme component to turn off legend plotting
    tt <- theme(legend.position="none",
                plot.title = element_text(hjust=-0.1, face="bold"))

    # TASK: Modify this code. I think it might be unreliable to depend on the
    #       automated segmentation. Inspect the results and decide.
    # OCT Segmentation minimum and maximum (from Heidelberg segmenation)
    if(!is.error(oct_seg_array) & !is.null(crop_to_heidelberg_segmentation)) {
        layer_y_min <- max(0, min(oct_seg_array[["z"]], na.rm = TRUE) - crop_to_heidelberg_segmentation[1])
        layer_y_max <- min(max(oct_seg_array[["z"]], na.rm = TRUE) + crop_to_heidelberg_segmentation[2],
                           oct$header$size_z)
    } else {
        layer_y_min <- 0
        layer_y_max <- oct$header$size_z
    }

    # Calculate grid center coordinates.
    # Adjust the coordinates for my 1-based system.
    if(!is.null(oct_center)) {
        center_x_voxel <- oct_center[["center"]][["x"]] + 1
        center_z_voxel <- b_n_seg[[as.character(oct_center[["center"]][["z"]] + 1)]]


        center_bscan <- oct$bscan_headers %>%
            filter(bscan == center_z_voxel)

        start_x_pixel <- center_bscan[["start_x_pixels"]]
        end_x_pixel <- center_bscan[["end_x_pixels"]]

        # NOTE: This will only work for horizontal scans.
        center_x <- start_x_pixel + (end_x_pixel - start_x_pixel) * (center_x_voxel / oct$header$size_x)
        center_y <- center_bscan[["start_y_pixels"]]
    }

    # Save the SLO separately if required
    if(omit_slo) {
        # Save the SLO here
        ggsave(p_slo + tt + theme_nothing(),
               file = paste0(output_path, "/", prefix,
                             paste(oct$header$patient_id,
                                   oct$header$vid,
                                   gsub(oct$header$visit_date,
                                        pattern = "-",
                                        replacement = ""),
                                   oct$header$scan_position,
                                   gsub(oct$header$id,
                                        pattern = "_",
                                        replacement = "-"),
                                   "IR.png",
                                   sep="_")),
               dpi = 300,
               width = 4,
               height = 4,
               units = "in")

        p_slo <- FALSE
    }

    1:oct$header$num_bscans %>%
        walk(~layout_plot_2(b_n = .x,
                            oct = oct,
                            p_slo = p_slo,
                            layer_y_max = layer_y_max,
                            layer_y_min = layer_y_min,
                            xml_file = xml_file,
                            oct_segmentation = oct_segmentation,
                            b_n_seg = b_n_seg,
                            center_file = center_file,
                            center_z_voxel = center_z_voxel,
                            center_x_voxel = center_x_voxel,
                            overlay_bscan_position = overlay_bscan_position,
                            overlay_heidelberg_segmentation = overlay_heidelberg_segmentation,
                            oct_seg_array = oct_seg_array,
                            base_name = paste0(prefix,
                                               paste(oct$header$patient_id,
                                               oct$header$vid,
                                               gsub(oct$header$visit_date,
                                                    pattern = "-",
                                                    replacement = ""),
                                               oct$header$scan_position,
                                               gsub(oct$header$id,
                                                    pattern = "_",
                                                    replacement = "-"),
                                               sep="_")),
                            output_path = output_path,
                            file_type = file_type,
                            tt = tt))


    if(return_results) {
        return(list(oct=oct, segmentation=oct_segmentation))
    }
}
