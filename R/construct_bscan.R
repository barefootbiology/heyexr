#' Construct a ggplot2 object containing the data for a given b-scan
#'
#' Constructs a ggplot2 object containing the data for a given b-scan
#'
#' @param oct a list containing the OCT data
#' @param bn the number of the b-scan of interest
#' @param layer_z_min vertical minimum constraint on the b-scan axis
#' @param layer_z_max vertical maximum constraint on the b-scan axix
#' @param low_color color for lowest values
#' @param high_color color for highest values
#' @param gamma value for gamma correction of b-scan values
#' @param scale_bars Should scale bars be drawn?
#' @param scale_length length of scale bars in microns
#' @param scale_color color of scale bar
#' @param inset_percentage percentage of image to inset the scale bar
#'
#' @return a ggplot2 object
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_raster aes theme_bw element_blank scale_y_reverse scale_x_continuous theme labs element_rect geom_path annotate
construct_bscan <- function(oct,
                            bn,
                            layer_z_max = NULL,
                            layer_z_min = NULL,
                            low_color = "black",
                            high_color = "white",
                            gamma = 0.33,
                            scale_bars = TRUE,
                            scale_length = 200,
                            scale_color = "white",
                            inset_percentage = 0.05) {

    # Build the scale bars:
    # Calculate the (x,y) for the bottom left corner
    bscan_x_0 <- oct$header$size_x * inset_percentage
    bscan_y_0 <- layer_z_max - ((layer_z_max - layer_z_min) * inset_percentage)

    bscan_x_length <- scale_length / (oct$header$scale_x * 1000)
    bscan_y_length <- scale_length / (oct$header$scale_z * 1000)

    # Construct a data.frame with the scale bar information
    bscan_scale_bars <- data.frame(x = c(bscan_x_0, bscan_x_0, bscan_x_0 + bscan_x_length),
                                   y = c(bscan_y_0 - bscan_y_length, bscan_y_0, bscan_y_0))

    # Construct the b-scan plot
    p_1 <- get_bscan(oct, bn) %>%
        mutate(intensity = ifelse(is.na(intensity), NA, intensity^gamma)) %>%
        ggplot(aes(x = x, y = z)) +
        geom_raster(aes(fill=intensity)) +
        scale_fill_continuous(low = low_color, high = high_color) +
        theme_bw() +
        scale_y_reverse(limits = c(layer_z_max, layer_z_min),
                        expand = c(0,0)) +
        scale_x_continuous(expand = c(0,0)) +
        theme(panel.grid=element_blank(),
              panel.background=element_rect(fill = "black"),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank()) +
        labs(x="x", y="z")

    if(scale_bars) {
        p_1 <- p_1 +
            geom_path(data = bscan_scale_bars,
                                  mapping = aes(x=x, y=y),
                                  color = scale_color, size = 0.5) +
            annotate("text",
                     x = bscan_x_0+4,
                     y = bscan_y_0-2,
                     label = paste(scale_length, "µm"),
                     color = scale_color,
                     hjust = 0,
                     vjust = 0,
                     size = 2)
    }

    return(p_1)
}
