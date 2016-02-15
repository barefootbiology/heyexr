# Plot data and segmentation --------------------------
render_oct_summary <- function(vol_path,
                               xml_path = NA,
                               return_results=FALSE,
                               recycle=NULL) {

    # TASK: Ensure that the render function will work with or without
    #       segmentation from OCT explorer.

    # TASK: Do not crop the image of the bscan along the z-axis using the
    #       XML segmentation results. IF you implement this function, crop
    #       using the Heidelberg segmentation.

    # From the results directory, construct the other file paths
    base_name <- basename(vol_path) %>%
        gsub(pattern=".VOL", replacement="", ignore.case = TRUE)
    output_path <- file.path(dirname(vol_path), "rendered_bscans")

    if(!dir.exists(output_path)) {
        dir.create(output_path)
    }

    # Load data ------------------------------------------
    oct <- NULL
    oct_segmentation <- NULL

    if(is.null(recycle)) {
        # Load raw data from VOL or RData file
        if(!file.exists(paste(vol_path, ".RData", sep=""))) {
            # Load the VOL data
            oct <- read_heyex(vol_path)
            save(oct, file = paste(vol_path, ".RData", sep=""))
        } else {
            load(paste(vol_path, ".RData", sep=""))
        }

        # Load segmentation from XML or RData file
        if(!file.exists(paste(xml_path, ".RData", sep=""))) {
            # Load the segmentation from OCT Explorer
            oct_segmentation <- read_segmentation_xml(xml_path)
            save(oct_segmentation, file = paste(xml_path, ".RData", sep=""))
        } else {
            load(paste(xml_path, ".RData", sep=""))
        }


    } else {
        oct <- recycle$oct
        oct_segmentation <- recycle$segmentation
    }

    p_slo <- construct_slo(oct)

    # TASK: Overlay this segmentation
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

    # OCT Segmentation minimum and maximum
#     layer_z_min <- max(0, min(oct_segmentation$layers[["value"]])-10)
#     layer_z_max <- min(max(oct_segmentation$layers[["value"]])+75,
#                        oct$header$size_z)
    layer_z_min <- max(0, min(oct_seg_array[["z"]], na.rm = TRUE) - 10)
    layer_z_max <- min(max(oct_seg_array[["z"]], na.rm = TRUE) + 75,
                       oct$header$size_z)

    # Plot each b-scan --------------------------
    # Initialize the progress bar
    pb <- txtProgressBar(min = 0,
                         max = oct$header$num_bscans,
                         style = 1,
                         title="Drawing b-scans...",
                         file=stderr())

    plot_list <- list()

    for (b_n in 1:oct$header$num_bscans) {

        # Construct the b-scan plot:
        # Perform gamma correction to lighten dark values.
        # Value of gamma recommended by author of Open Heyex plugin for
        # ImageJ.

        p_1 <- construct_bscan(oct, b_n, layer_z_max = layer_z_max,
                                       layer_z_min = layer_z_min)

        # Overlay Iowa Reference Algorithms segmentation
        # TASK: Make this code contingent on the XML map
        p_1_l <- p_1 +
            geom_line(data=oct_segmentation$layers %>%
                          filter(bscan_id == b_n_seg[as.character(b_n)]),
                      mapping = aes(x=ascan_id,
                                    y=value,
                                    group=as.factor(layer_z_order),
                                    color=as.factor(layer_z_order)),
                      size=0.25, alpha=0.6) +
            scale_color_brewer(name="boundary", palette = "Spectral")

        # Overlay Heidelberg segmentation (just for kicks)
        p_1_l2 <- p_1 +
            geom_line(data = oct_seg_array %>% dplyr::filter(b_scan == b_n),
                      mapping = aes(group=as.factor(seg_layer),
                                    color=as.factor(seg_layer))) +
            scale_colour_manual(guide = 'none',
                                values = c("1"="blue",
                                           "2"="green"))

        # Overlay the position of the b-scans on the SL
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

        # Layout the plots
        p_layout <- arrangeGrob(arrangeGrob(p_1_l + tt, p_1_l2 + tt, nrow=2),
                                 arrangeGrob(p_slo_1 + tt, nrow=2),
                                 ncol=2, widths = c(3,2))

        # Save the plot in a list
        # plot_list[[b_n]] <- p_layout

        # Save the plot as a layout
        file_out <- paste(output_path, "/", base_name, "_", sprintf("%03d", b_n), ".pdf", sep="")
        ggsave(filename = file_out, plot = p_layout,
               units = "in", width = 12, height = 8, dpi = 300)

        # Update the progress bar
        setTxtProgressBar(pb, b_n)

    }

#     # Save a single PDF of all the scans
#     ggsave(filename = paste(output_path, "/", base_name, "_", "compiled", ".pdf", sep=""),
#            do.call(marrangeGrob, c(plot_list, list(nrow=1, ncol=1))),
#            units = "in", width = 12, height = 8, dpi = 300)

    if(return_results) {
        return(list(oct=oct, segmentation=oct_segmentation))
    }
}
