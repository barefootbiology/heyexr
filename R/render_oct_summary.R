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
render_oct_summary <- function(vol_file,
                               xml_file = NULL,
                               center_file = NULL,
                               out_dir = "rendered_bscans",
                               return_results=FALSE,
                               n_cores = 1,
                               overlay_heidelberg_segmentation = TRUE,
                               crop_to_heidelberg_segmentation = c(10, 75)) {

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
    oct_seg_array <- get_segmentation(oct)

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
    layer_y_min <- max(0, min(oct_seg_array[["z"]], na.rm = TRUE) - crop_to_heidelberg_segmentation[1])
    layer_y_max <- min(max(oct_seg_array[["z"]], na.rm = TRUE) + crop_to_heidelberg_segmentation[2],
                       oct$header$size_z)

    # Calculate grid center stuff
    # Adjust the coordinates for my 1-based system
    center_x_voxel <- oct_center[["center"]][["x"]] + 1
    center_z_voxel <- oct_center[["center"]][["z"]] + 1

    center_bscan <- oct$bscan_headers %>%
        filter(bscan == center_z_voxel)

    start_x_pixel <- center_bscan[["start_x_pixels"]]
    end_x_pixel <- center_bscan[["end_x_pixels"]]

    # NOTE: This will only work for horizontal scans.
    center_x <- start_x_pixel + (end_x_pixel - start_x_pixel) * (center_x_voxel / oct$header$size_x)
    center_y <- center_bscan[["start_y_pixels"]]

    # Label the output files in the reverse order as the bscan IDs
    reverse_order <- oct$header$num_bscans:1

    # Initialize the progress bar
#     pb <- txtProgressBar(min = 0,
#                          max = oct$header$num_bscans,
#                          style = 1,
#                          title="Drawing b-scans...",
#                          file=stderr())

#     # Create a list to store each rendered plot
#     plot_list <- list()

    #n_cores <- detectCores() - 1
#     cl <- makeCluster(n_cores, type = "FORK")
#
#     clusterCall(cl, function() {
#         library(magrittr);
#         library(heyexr);
#         library(dplyr);
#         library(ggplot2);
#         library(gridExtra); })
#     clusterEvalQ(cl, library(heyexr))
#     clusterEvalQ(cl, library(dplyr))
#     clusterEvalQ(cl, library(ggplot2))
#     clusterEvalQ(cl, library(gridExtra))

    # Plot each b-scan in parallel --------------------------
#     plot_list <- parLapply(cl, 1:oct$header$num_bscans,
#                            function(b_n) {
    lapply(1:oct$header$num_bscans, function(b_n) {
    #for (b_n in 1:oct$header$num_bscans) {

        # Construct the b-scan plot:
        # Perform gamma correction to lighten dark values.
        # Value of gamma recommended by author of Open Heyex plugin for
        # ImageJ.

        p_1 <- construct_bscan(oct, b_n, layer_y_max = layer_y_max,
                                       layer_y_min = layer_y_min)

        # If an XML file was provided,
        # overlay Iowa Reference Algorithms segmentation on the top b-scan.
        if(!is.null(xml_file)) {
            p_1_l <- p_1 +
                geom_line(data=oct_segmentation$layers %>%
                              filter(bscan_id == b_n_seg[as.character(b_n)]),
                          mapping = aes(x=ascan_id,
                                        y=value + 1, # TESTING!!!!
                                        group=as.factor(layer_y_order),
                                        color=as.factor(layer_y_order)),
                          alpha=0.6) +
                scale_color_brewer(name="boundary", palette = "Paired")
        } else {
            p_1_l <- p_1
        }

        if(!is.null(center_file)) {
            if(b_n == center_z_voxel) {
                p_1_l <- p_1_l +
                    geom_vline(xintercept = center_x_voxel,
                               color = "green",
                               alpha = 0.5,
                               linetype = "dotted")
            }
        }

        # Overlay Heidelberg segmentation on the lower b-scan plot
        if(overlay_heidelberg_segmentation) {
            n_segments <- oct_seg_array %>%
                dplyr::filter(b_scan == b_n) %>%
                select(seg_layer) %>%
                distinct() %>%
                collect %>%
                .[["seg_layer"]]

            segmentation_layer_value <- c("1"="red","2"="blue","3"="green")[as.character(n_segments)]

            p_1_l2 <- p_1 +
                geom_line(data = oct_seg_array %>% dplyr::filter(b_scan == b_n),
                          mapping = aes(group=as.factor(seg_layer),
                                        color=as.factor(seg_layer)),
                          alpha = 0.5) +
                scale_color_manual(guide = 'none',
                                    values = segmentation_layer_value)
        } else {
            p_1_l2 <- p_1
        }

        # Overlay the position of the b-scans on the top SLO
        p_slo_1 <- p_slo +
            geom_segment(data = oct$bscan_headers %>%
                             mutate(is_current = ifelse(bscan == b_n,
                                                        "current",
                                                        "other")),
                         mapping = aes(x = start_x_pixels,
                                       xend = end_x_pixels,
                                       y = start_y_pixels,
                                       yend = end_y_pixels,
                                       color = is_current),
                         alpha = 0.5) +
            scale_color_manual(values = c("current"="green",
                                          "other"="darkgreen"),
                               guide = 'none')

        # Add in the grid center information if available
        if(!is.null(center_file)) {
            p_slo_1 <- p_slo_1 +
                annotate("point",
                         x = center_x,
                         y = center_y,
                         color = "green",
                         alpha = 0.5)
        }

        # Layout the plots
        p_layout <- arrangeGrob(arrangeGrob(p_1_l + tt, p_1_l2 + tt, nrow=2),
                                 arrangeGrob(p_slo_1 + tt, p_slo + tt, nrow=2),
                                 ncol=2, widths = c(3,2))

        # Save the plot in a list
        # plot_list[[b_n]] <- p_layout

        # Save the plot as a layout
        file_out <- paste(output_path, "/", base_name, "_", sprintf("%03d", reverse_order[b_n]), ".pdf", sep="")
        ggsave(filename = file_out, plot = p_layout,
               units = "in", width = 12, height = 8, dpi = 300)

        # Update the progress bar
        # setTxtProgressBar(pb, b_n)

    })

    #stopCluster(cl)


#     # Save a single PDF of all the scans
#     ggsave(filename = paste(output_path, "/", base_name, "_", "compiled", ".pdf", sep=""),
#            do.call(marrangeGrob, c(plot_list, list(nrow=1, ncol=1))),
#            units = "in", width = 12, height = 8, dpi = 300)

    if(return_results) {
        return(list(oct=oct, segmentation=oct_segmentation))
    }
}
