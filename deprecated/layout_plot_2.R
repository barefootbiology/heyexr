#' Layout plots
#'
#' UPDATE
#'
#' @return a list containing the OCT and segmentation data (if requested)
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter collect distinct select
#' @importFrom ggplot2 scale_color_brewer element_text theme ggsave scale_color_manual geom_line geom_segment geom_tile geom_vline aes ggplot annotate ggtitle ggplotGrob
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid textGrob arrow
#' @importFrom parallel parLapply makeCluster clusterCall stopCluster
#' @importFrom purrr walk
layout_plot_2 <- function(bscan_id, oct, p_slo, layer_y_max, layer_y_min, xml_file,
                          oct_segmentation, bscan_id_seg, center_file,
                          center_z_voxel, center_x_voxel,
                          overlay_bscan_position,
                          overlay_heidelberg_segmentation, oct_seg_array,
                          base_name, output_path, file_type,
                          tt) {

    # Label the output files in the reverse order as the bscan IDs
    reverse_order <- oct$header$num_bscans:1

    # Function from http://adv-r.had.co.nz/Exceptions-Debugging.html
    is.error <- function(x) inherits(x, "try-error")

    # Construct the b-scan plot:
    # Perform gamma correction to lighten dark values.
    # Value of gamma recommended by author of Open Heyex plugin for
    # ImageJ.


    p_1 <- construct_bscan(oct, bscan_id, layer_y_max = layer_y_max,
                           layer_y_min = layer_y_min,
                           contrast_correction = spline_correction) +
        labs(x = "", y = "") +
        tt +
        theme_nude()

    # If an XML file was provided,
    # overlay Iowa Reference Algorithms segmentation on the top b-scan.
    if(!is.null(xml_file)) {
        p_1_l <- p_1 +
            geom_line(data=oct_segmentation$layers %>%
                          filter(bscan_id == bscan_id_seg[as.character(bscan_id)]),
                      mapping = aes(x=ascan_id,
                                    y=value + 1, # TESTING!!!!
                                    group=as.factor(surface_id),
                                    color=as.factor(surface_id)),
                      alpha=0.6) +
            scale_color_brewer(name="boundary", palette = "Paired")
    } else {
        p_1_l <- p_1
    }

    # Add a vertical line to indicate the position of the
    # manually annotated center (e.g., fovea).
    if(!is.null(center_file)) {
        if(bscan_id == center_z_voxel) {
            p_1_l <- p_1_l +
                geom_vline(xintercept = center_x_voxel,
                           color = "green",
                           alpha = 0.5,
                           linetype = "dotted")
        }
    }

    # Overlay Heidelberg segmentation on the lower b-scan plot
    if(overlay_heidelberg_segmentation & !is.error(oct_seg_array)) {
        n_segments <- oct_seg_array %>%
            dplyr::filter(bscan_id == bscan_id) %>%
            select(seg_layer) %>%
            distinct() %>%
            collect %>%
            .[["seg_layer"]]

        segmentation_layer_value <- c("1"="red","2"="blue","3"="green")[as.character(n_segments)]

        p_1_l2 <- p_1 +
            geom_line(data = oct_seg_array %>% dplyr::filter(bscan_id == bscan_id),
                      mapping = aes(group=as.factor(seg_layer),
                                    color=as.factor(seg_layer)),
                      alpha = 0.5) +
            scale_color_manual(guide = 'none',
                               values = segmentation_layer_value)
    } else {
        p_1_l2 <- p_1
    }

    p_layout <- NULL
    width <- NULL

    # If you are to include the SLO
    if(p_slo != FALSE) {
        # And if the b-scans should overlay the IR image
        if(overlay_bscan_position) {
            # Overlay the position of the b-scans on the top SLO
            p_slo_1 <- p_slo +
                # Draw the other b-scans but not the current one
                geom_segment(data = oct$bscan_headers %>%
                                 filter(bscan_id != bscan_id),
                             mapping = aes(x = start_x_pixels,
                                           xend = end_x_pixels,
                                           y = start_y_pixels,
                                           yend = end_y_pixels),
                             color = "darkgreen",
                             alpha = 0.5) +

                # Draw the current b-scan
                geom_segment(data = oct$bscan_headers %>%
                                 filter(bscan_id == bscan_id),
                             mapping = aes(x = start_x_pixels,
                                           xend = end_x_pixels,
                                           y = start_y_pixels,
                                           yend = end_y_pixels),
                             color = "green",
                             alpha = 0.9,
                             arrow = arrow(angle = 15, length = unit(0.1, "inches"), type = "closed")) +

                labs(x = "", y = "") +
                tt +
                theme_nothing()
        } else {
            p_slo_1 <- p_slo +
                labs(x = "", y = "") +
                tt +
                theme_nothing()

        }

        # Add in the grid center information if available
        if(!is.null(center_file)) {
            p_slo_1 <- p_slo_1 +
                annotate("point",
                         x = center_x,
                         y = center_y,
                         color = "green",
                         alpha = 0.5)
        }

        # Set the height of the b-scan to that of the SLO
        g_slo_1 <- ggplotGrob(p_slo_1)

        g_1_l <- ggplotGrob(p_1_l)

        g_1_l$heights <- g_slo_1$heights

        # Layout the plots
        p_layout <- arrangeGrob(g_slo_1 ,
                                g_1_l ,
                                ncol=2,
                                widths = c(4, 6))

        width <- 10

    } else {
        p_layout <- p_1_l
        width <- 10 - 4
    }

    # Save the plot as a layout
    file_out <- paste(output_path, "/", base_name, "_",
                      sprintf("%03d", reverse_order[bscan_id]), ".",
                      file_type, sep="")

    ggsave(filename = file_out, plot = p_layout,
           units = "in", width = width, height = 4, dpi = 300)

}
