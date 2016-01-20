# Plot data and segmentation --------------------------
render_oct_summary <- function(input_path, return_results=TRUE,
                               recycle=NULL) {

    # From the results directory, construct the other file paths
    output_path <- file.path(input_path, "rendered_bscans")

    base_name <- basename(input_path)
    vol_path <- paste(input_path, "/", base_name, "_OCT_Iowa.vol", sep="")
    xml_path <- paste(input_path, "/", base_name, "_Surfaces_Iowa.xml", sep="")

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

    # TASK: Determine placement of scale angle on the SLO
    # start x

    # Calculate the (x,y) for the bottom left corner
    scale_length <- 500
    inset_percentage <- 1/20
    x_0 <- oct$header$size_x_slo * inset_percentage
    y_0 <- oct$header$size_y_slo * inset_percentage
    x_length <- scale_length / (oct$header$scale_x_slo * 1000)
    y_length <- scale_length / (oct$header$scale_y_slo * 1000)

    slo_scale_bars <- data.frame(line_slo = c("slo_horizontal", "slo_vertical"),
               x = c(x_0, x_0),
               xend = c(x_0 + x_length, x_0),
               y = c(y_0, y_0),
               yend = c(y_0, y_0 + y_length))

    # Construct the SLO plot
    p_slo <- get_slo(oct) %>%
        ggplot(aes(x=x, y=y)) +
        geom_raster(aes(fill = z)) +
        scale_fill_continuous(low = "black", high = "white") +
        theme_bw() +
        theme(axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank()) +
        coord_fixed() +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        geom_segment(data = slo_scale_bars,
                     mapping = aes(x=x, xend=xend, y=y, yend=yend),
                     color = "white", size = 0.5) +
        annotate("text",
                 x = x_0 + 10,
                 y = y_0 + 10,
                 label = paste(scale_length, "µm"),
                 color = "white",
                 hjust = 0,
                 vjust = 0,
                 size = 2)

    cat("1\n")

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

    # OCT Segementation minimum and maximum
    layer_z_min <- max(0, min(oct_segmentation$layers[["value"]])-10)
    layer_z_max <- min(max(oct_segmentation$layers[["value"]])+75,
                       oct$header$size_z)

    # Plot each b-scan --------------------------
    # Initialize the progress bar
    pb <- txtProgressBar(min = 0,
                         max = oct$header$num_bscans,
                         style = 3,
                         title="Drawing b-scans...",
                         file=stderr())
    cat("2\n")

    # Build the scale bars
    # Calculate the (x,y) for the bottom left corner
    bscan_scale_length <- 200
    bscan_inset_percentage <- 1/20
    bscan_x_0 <- oct$header$size_x * bscan_inset_percentage
    # bscan_y_0 <- oct$header$size_z * bscan_inset_percentage
    bscan_y_0 <- layer_z_max - ((layer_z_max - layer_z_min) * bscan_inset_percentage)

    bscan_x_length <- bscan_scale_length / (oct$header$scale_x * 1000)
    bscan_y_length <- bscan_scale_length / (oct$header$scale_z * 1000)

    bscan_scale_bars <- data.frame(line = c("horizontal", "vertical"),
                                 x = c(bscan_x_0, bscan_x_0),
                                 xend = c(bscan_x_0 + bscan_x_length, bscan_x_0),
                                 y = c(bscan_y_0, bscan_y_0),
                                 yend = c(bscan_y_0, bscan_y_0 - bscan_y_length))

    cat("3\n")

    for (b_n in 1:oct$header$num_bscans) {
        # Perform gamma correction to lighten dark values.
        # Value of gamma recommended by author of Open Heyex plugin for
        # ImageJ.
        oct_bscan_1 <- get_bscan(oct, b_n) %>%
            mutate(intensity = ifelse(is.na(intensity), NA, intensity^0.33))

        cat("4\n")

        # Construct the b-scan plot
        p_1 <- oct_bscan_1  %>%
            ggplot(aes(x = x, y = z)) +
            geom_raster(aes(fill=intensity)) +
            scale_fill_continuous(low = "black", high = "white") +
            theme_bw() +
            # geom_point(data = oct_seg_array %>% filter(b_scan == b_n),
            #           mapping=aes(x = x, y = y, color = as.factor(seg_layer)),
            #          alpha = 0.3, shape = 15, size = 1) +
            # scale_color_brewer(palette = "Set1") +
            scale_y_reverse(limits = c(layer_z_max, layer_z_min),
                            expand = c(0,0)) +
            scale_x_continuous(expand = c(0,0)) +
            theme(panel.grid=element_blank(),
                  panel.background=element_rect(fill = "black"),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank()) +
            labs(x="x", y="z") +
            geom_segment(data = bscan_scale_bars,
                         mapping = aes(x=x, xend=xend, y=y, yend=yend),
                         color = "white", size = 0.5) +
            annotate("text",
                     x = bscan_x_0+4,
                     y = bscan_y_0-2,
                     label = paste(bscan_scale_length, "µm"),
                     color = "white",
                     hjust = 0,
                     vjust = 0,
                     size = 2)

        cat("5\n")

        p_1_l <- p_1 +
            geom_line(data=oct_segmentation$layers %>%
                          filter(bscan_id == b_n_seg[as.character(b_n)]),
                      mapping = aes(x=ascan_id,
                                    y=value,
                                    group=as.factor(layer_z_order),
                                    color=as.factor(layer_z_order)),
                      size=0.25, alpha=0.6) +
            scale_color_brewer(name="boundary", palette = "Spectral")

        cat("5.1\n")

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
        p_layout <- grid.arrange(arrangeGrob(p_1_l + tt, p_1 + tt, nrow=2),
                                 arrangeGrob(p_slo_1 + tt, nrow=2),
                                 ncol=2, widths = c(3,2))

        cat("6\n")

        # Save the plot as a layout
        png(file=paste(output_path, "/", base_name,"_", b_n, ".png", sep=""),
            width = 12*200, height = 8*200, res=200)
        grid.draw(p_layout)
        dev.off()

        # Update the progress bar
        setTxtProgressBar(pb, b_n)

    }

    if(return_results) {
        return(list(oct=oct, segmentation=oct_segmentation))
    }

    return()
}
