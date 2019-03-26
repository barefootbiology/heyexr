#!/usr/bin/env Rscript

library(argparser)
library(heyexr)
library(ggplot2)
library(purrr)
library(tibble)
library(dplyr)


segment_angle <- function(x, y, xend, yend) {
    # Function based on:
    # https://stackoverflow.com/questions/7586063/how-to-calculate-the-angle-between-a-line-and-the-horizontal-axis

    # Horizontal should be angle 0
    # Pointed up should be angle 90
    # Pointed in reverse should be 180

    delta_x <- xend - x
    delta_y <- yend - y

    angle <- atan2(delta_y , delta_x) * 180 / pi

    return(angle)
}

change_angle <- function(x) {
    if (x < 0) {
        return(x * -1)
    } else if (x > 0) {
        return(360 - x)
    }

    return(x)
}



# PARSE PARAMETERS -------------------------------------------------------------
p <- arg_parser("Render OCT SLO and b-scans")

p <- add_argument(p, "--vol",
                  help = "Heidelberg VOL file to parse")

p <- add_argument(p, "--outdir",
                  help="Directory to write results",
                  default = ".")

p <- add_argument(p, "--type",
                  help="File type",
                  default = "png")

argv <- parse_args(p)



# LOAD THE DATA ----------------------------------------------------------------
oct <- read_vol(vol_file = argv$vol)

p_slo_scale <- construct_slo(oct, scale_bar = TRUE)

p_slo_0 <- construct_slo(oct, scale_bar = FALSE)

base_name <- paste(oct$header$patient_id,
                          # oct$header$vid,
                          gsub(oct$header$visit_date,
                               pattern = "-",
                               replacement = ""),
                          oct$header$scan_position,
                          gsub(oct$header$id,
                               pattern = "_",
                               replacement = "-"),
                          sep="_")

output_path <- argv$outdir

file_type <- argv$type

# Input parameters
aspect_ratio <- 1.86
height_in <- 6

# Calculated parameters
dpi <- 100

width_in <- height_in * aspect_ratio

width_px <- width_in * dpi
height_px <- height_in * dpi

inset_size <- 100


# SAVE THE SLO -----------------------------------------------------------------
slo_out <- paste0(output_path, "/", base_name, "_IR.", file_type)

# Save the SLO with the scale bar
ggsave(filename = slo_out, plot = p_slo_scale + theme_nude(),
       width = 8, height = 8, units = "in")

# #-------------------------------------------------------------------------------
# # TASK: Revise the scale bar placement.
# # Build the scale bars:
# # Calculate the (x,y) for the bottom left corner
# scale_length <- 200
# #inset_percentage <- 0.05
# layer_y_max <- oct$header$size_z
# layer_y_min <- 1
#
# bscan_x_0 <- oct$header$size_x * 0.25
# bscan_y_0 <- layer_y_max - ((layer_y_max - layer_y_min) * 0.05)
#
# bscan_x_length <- scale_length / (oct$header$scale_x * 1000)
# bscan_y_length <- scale_length / (oct$header$scale_z * 1000)
#
# # Construct a data.frame with the scale bar information
# bscan_scale_bars <- data.frame(x = c(bscan_x_0, bscan_x_0, bscan_x_0 + bscan_x_length),
#                                y = c(bscan_y_0 - bscan_y_length, bscan_y_0, bscan_y_0))
# #-------------------------------------------------------------------------------


for(b_n in 1:oct$header$num_bscans) {

    # Get the SLO
    # Overlay the positions of the b-scans in green

    p_slo <- p_slo_0 +
        # Draw the other b-scans but not the current one
        geom_segment(data = oct$bscan_headers %>%
                         filter(bscan_id != b_n),
                     mapping = aes(x = start_x_pixels,
                                   xend = end_x_pixels,
                                   y = start_y_pixels,
                                   yend = end_y_pixels),
                     color = "darkgreen",
                     alpha = 0.5) +

        # Draw the current b-scan
        geom_segment(data = oct$bscan_headers %>%
                         filter(bscan_id == b_n),
                     mapping = aes(x = start_x_pixels,
                                   xend = end_x_pixels,
                                   y = start_y_pixels,
                                   yend = end_y_pixels),
                     color = "green",
                     alpha = 0.9,
                     arrow = arrow(angle = 15, length = unit(0.05, "inches"),
                                   type = "closed")) +
        theme_nude()

    # # Use the Heidelberg segmentation to find the
    # max_y <- max(get_segmentation(oct)$z, na.rm = TRUE)
    # min_y <- min(get_segmentation(oct)$z, na.rm = TRUE)

    # Get the first b-scan
    p_bscan <- construct_bscan(oct = oct, bscan_id = b_n,
                               # layer_y_max = max_y + 100,
                               layer_y_max = oct$header$size_z,
                               layer_y_min = 1,
                               contrast_correction = spline_correction,
                               scale_bars = TRUE) +
        theme_nude()

    p_plot <- tibble(x = c(1, width_px), y = c(1, height_px)) %>%
        ggplot(aes(x = x, y = y)) +
        geom_point(color = NA) +
        annotation_custom(ggplotGrob(p_bscan),
                          xmin = 1, xmax = width_px,
                          ymin = 1, ymax = height_px) +
        # Add the SLO as an inset
        annotation_custom(ggplotGrob(p_slo),
                          xmin = width_px - inset_size + 1, xmax = width_px,
                          ymin = 1, ymax = inset_size) +
        theme_nude() +
        theme(panel.spacing = unit(0, "mm"),
              plot.background = element_rect(fill = "black"),
              panel.background = element_rect(fill = "black")) +
        coord_cartesian(expand = FALSE)

    # if(scale_bars) {
    #     p_1 <- p_1 +
    #         geom_path(data = bscan_scale_bars,
    #                   mapping = aes(x=x, y=y),
    #                   color = scale_color, size = 0.5) +
    #         annotate("text",
    #                  x = bscan_x_0+4,
    #                  y = bscan_y_0-2,
    #                  label = deparse(bquote(.(scale_length)~mu*"m")),
    #                  color = scale_color,
    #                  hjust = 0,
    #                  vjust = 0,
    #                  size = 2,
    #                  parse = TRUE)
    # }

    # Compute the angle of the b-scan, relative to the horizontal axis.
    b_angle <- oct$bscan_headers %>%
        filter(bscan_id == b_n) %>%
        mutate(angle = segment_angle(x = start_x, y = start_y,
                                     xend = end_x, yend = end_y) %>%
                   round()) %>%
        collect() %>%
        .[["angle"]] %>%
        change_angle()

    # Construct a file name for the b-scan
    file_out <- paste0(output_path, "/", base_name, "_",
                      sprintf("%03d", b_n),
                      "_", sprintf("%03d", b_angle),
                      ".", file_type)

    ggsave(plot = p_plot, filename = file_out, device = file_type,
           width = width_in, height = height_in, units = "in")
}

