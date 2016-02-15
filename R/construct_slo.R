#' Construct a ggplot2 object containing the data for an SLO
#'
#' Construct a ggplot2 object containing the data for an SLO
#'
#' @param oct a list containing the OCT data
#' @param low_color color for lowest values
#' @param high_color color for highest values
#' @param scale_bars Should scale bars be drawn?
#' @param scale_length length of scale bars in microns
#' @param scale_color color of scale bar
#' @param inset_percentage percentage of image to inset the scale bar
#'
#' @return a ggplot2 object
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom grid unit
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_raster element_blank theme_bw theme scale_fill_continuous coord_fixed scale_x_continuous scale_y_continuous labs geom_path annotate
construct_slo <- function(oct,
                          low_color = "black",
                          high_color = "white",
                          scale_bar = TRUE,
                          scale_length = 500,
                          scale_color = "white",
                          inset_percentage = 0.05) {

    # Determine placement of scale angle on the SLO:
    # Calculate the (x,y) for the bottom left corner
    x_0 <- oct$header$size_x_slo * inset_percentage
    y_0 <- oct$header$size_y_slo * (1-inset_percentage)
    x_length <- scale_length / (oct$header$scale_x_slo * 1000)
    y_length <- scale_length / (oct$header$scale_y_slo * 1000)

    # Construct a data.frame that holds the scale bar coordinates
    slo_scale_bars <- data.frame(x = c(x_0, x_0, x_0 + x_length),
                                 y = c(y_0 - y_length, y_0, y_0))

    # Construct the SLO plot
    p_slo <- get_slo(oct) %>%
        ggplot(aes(x=x, y=y)) +
        geom_raster(aes(fill = z)) +
        scale_fill_continuous(low = low_color, high = high_color) +
        theme_bw() +
        theme(axis.ticks = element_blank(),
              axis.ticks.length = unit(0, "null"),
              panel.grid = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none",
              plot.margin = unit(c(0,0,0,0), "cm")) +
        coord_fixed() +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0), trans = "reverse") +
        labs(x=NULL, y=NULL)

    # Add a scale bar if requested
    if (scale_bar) {
        p_slo <- p_slo +
            geom_path(data = slo_scale_bars,
                         mapping = aes(x=x, y=y),
                         color = scale_color, size = 0.5) +
            annotate("text",
                     x = x_0 + 10,
                     y = y_0 - 10,
                     label = paste(scale_length, "µm"),
                     color = "white",
                     hjust = 0,
                     vjust = 0,
                     size = 2)
    }

    # Return the ggplot2 plot object
    return(p_slo)
}
