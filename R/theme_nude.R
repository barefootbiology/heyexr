#' Render a ggplot2 plot with only the data.
#'
#' \code{theme_nude} themes a ggplot2 plot with only the data. Remember to remove any padding by
#' also adding `+ scale_x_continous(expand = c(0, 0)) + scale_y_continous(expand = c(0, 0))`.
#' This function is useful when overlaying raster data on en face and B-scan
#' images.
#'
#' @export
#' @importFrom ggplot2 theme_grey theme element_rect element_blank margin unit
theme_nude <- function (base_size = 11,
                        base_family = "",
                        base_line_size = base_size/22,
                        base_rect_size = base_size/22) {
  theme_grey(base_size = base_size,
             base_family = base_family,
             base_line_size = base_line_size,
             base_rect_size = base_rect_size) %+replace%
    theme(panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks.length = unit(0, "mm"),
          legend.position = "none",
          complete = TRUE)
}
