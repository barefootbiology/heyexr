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
#' @param draw_margins Should the plot include margins and axis labels?
#'
#' @return a ggplot2 object
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom grid unit
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_raster element_blank theme_bw theme scale_fill_continuous coord_fixed scale_x_continuous scale_y_continuous labs geom_path annotate
#' @importFrom rlang .data
construct_slo <- function(oct,
                          low_color = "black",
                          high_color = "white",
                          scale_bar = TRUE,
                          scale_length = 200,
                          scale_color = "white",
                          inset_percentage = 0.05,
                          draw_margins = TRUE) {

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
        ggplot(aes(x=.data$x, y=.data$y)) +
        geom_raster(aes(fill = .data$z)) +
        # TASK: Consider mapping the high and low value to the min and max
        #       values on the underlying data, not just the values that might
        #       be present in a given scan.
        scale_fill_continuous(guide = "none",
                              low = low_color,
                              high = high_color) +
        theme_bw() +
        coord_fixed() +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0), trans = "reverse") +
        theme(axis.ticks = element_blank(),
              axis.ticks.length = unit(0, "null"),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

    # Remove margins if requested
    if (!draw_margins) {
        p_slo <- p_slo +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.position = "none",
                  plot.margin = unit(c(0,0,0,0), "cm")) +
            labs(x=NULL, y=NULL)
    }

    # Add a scale bar if requested
    if (scale_bar) {
        p_slo <- p_slo +
            geom_path(data = slo_scale_bars,
                         mapping = aes(x=.data$x, y=.data$y),
                         color = scale_color, size = 0.5) +
            annotate("text",
                     x = x_0 + 10,
                     y = y_0 - 10,
                     label = deparse(bquote(.(scale_length)~mu*"m")),
                     color = "white",
                     hjust = 0,
                     vjust = 0,
                     size = 2,
                     parse = TRUE)
    }

    # Return the ggplot2 plot object
    return(p_slo)
}
