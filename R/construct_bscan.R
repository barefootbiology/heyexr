#' Construct a ggplot2 object containing the data for a given b-scan.
#'
#' \code{construct_bscan} builds a ggplot2 object containing the data for a
#' given b-scan.
#'
#' @param oct A list containing the OCT data.
#' @param bscan_id The number of the b-scan of interest.
#' @param layer_y_min Vertical minimum constraint on the b-scan axis.
#' @param layer_y_max Vertical maximum constraint on the b-scan axix.
#' @param low_color Color for lowest values.
#' @param high_color Color for highest values.
#' @param scale_bars Boolean indicating if scale bars be drawn.
#' @param scale_length Length of scale bars in microns.
#' @param scale_color Color of scale bar.
#' @param inset_percentage Percentage of image to inset the scale bar.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_raster aes theme_bw element_blank scale_y_reverse scale_x_continuous theme labs element_rect geom_path annotate
#' @importFrom rlang .data
construct_bscan <- function(oct,
                            bscan_id,
                            layer_y_max = oct$header$size_z,
                            layer_y_min = 0,
                            low_color = "black",
                            high_color = "white",
                            contrast_correction = spline_correction,
                            na_intensity = 0,
                            scale_bars = TRUE,
                            scale_length = 200,
                            scale_color = "white",
                            inset_percentage = c(x = 0.025, y = 0.05)) {

    # Build the scale bars:
    # Calculate the (x,y) for the bottom left corner
    bscan_x_0 <- oct$header$size_x * inset_percentage[[1]]
    bscan_y_0 <- layer_y_max - ((layer_y_max - layer_y_min) * inset_percentage[[2]])

    bscan_x_length <- scale_length / (oct$header$scale_x * 1000)
    bscan_y_length <- scale_length / (oct$header$scale_z * 1000)

    # Construct a data.frame with the scale bar information
    bscan_scale_bars <- data.frame(x = c(bscan_x_0, bscan_x_0, bscan_x_0 + bscan_x_length),
                                   y = c(bscan_y_0 - bscan_y_length, bscan_y_0, bscan_y_0))

    # Construct the b-scan plot
    p_1 <- get_bscan(oct, bscan_id) %>%
        # Replace any missing values
        mutate(intensity = ifelse(is.na(.data$intensity),
                                  na_intensity,
                                  .data$intensity)) %>%
        # # Apply contrast correction
        mutate(intensity = contrast_correction(.data$intensity)) %>%
        ggplot(aes(x = .data$x, y = .data$z)) +
        geom_raster(aes(fill = .data$intensity)) +
        scale_fill_continuous(guide = "none",
                              low = low_color,
                              high = high_color) +
        theme_bw() +
        scale_y_reverse(limits = c(layer_y_max, layer_y_min),
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
                                  mapping = aes(x=.data$x, y=.data$y),
                                  color = scale_color, size = 0.5) +
            annotate("text",
                     x = bscan_x_0+4,
                     y = bscan_y_0-2,
                     label = deparse(bquote(.(scale_length)~mu*"m")),
                     color = scale_color,
                     hjust = 0,
                     vjust = 0,
                     size = 2,
                     parse = TRUE)
    }

    return(p_1)
}
