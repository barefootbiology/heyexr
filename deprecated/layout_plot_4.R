layout_plot_4 <- function(b_n, oct, p_slo, layer_y_max, layer_y_min, xml_file,
                          oct_segmentation, b_n_seg, center_file,
                          center_z_voxel, center_x_voxel,
                          overlay_heidelberg_segmentation, oct_seg_array,
                          base_name, output_path, file_type,
                          tt) {

    # Label the output files in the reverse order as the bscan IDs
    reverse_order <- oct$header$num_bscans:1

    # Function from http://adv-r.had.co.nz/Exceptions-Debugging.html
    is.error <- function(x) inherits(x, "try-error")

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

    # Add a vertical line to indicate the position of the
    # manually annotated center (e.g., fovea).
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
    if(overlay_heidelberg_segmentation & !is.error(oct_seg_array)) {
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
    file_out <- paste(output_path, "/", base_name, "_",
                      sprintf("%03d", reverse_order[b_n]), ".",
                      file_type, sep="")
    ggsave(filename = file_out, plot = p_layout,
           units = "in", width = 12, height = 8, dpi = 300)

    # Update the progress bar
    # setTxtProgressBar(pb, b_n)

}
