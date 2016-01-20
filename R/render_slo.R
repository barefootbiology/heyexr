render_slo <- function(file, out_dir=".", type="png", size = 4, units ="in", ...) {
    # For a give VOL file, read the SLO information.
    oct <- read_heyex(file, header_slo_only=TRUE)

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

    # Render the output as a PNG file in the specificed directory
    p_slo <- get_slo(oct) %>%
        ggplot(aes(x=x, y=y)) +
        geom_raster(aes(fill = z)) +
        scale_fill_continuous(low = "black", high = "white") +
        theme_bw() +
        theme(axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "none") +
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


    # Extract the basename from the VOL file
    file_base = basename(file) %>%
        gsub(pattern = ".vol$", replacement = "",
             perl = TRUE, ignore.case = TRUE)

    # Save the ggplot using select device (file type)
    ggsave(plot = p_slo,
           filename = paste(out_dir, "/", file_base,
                            ".", type, sep=""),
           device = type,
           width = size,
           height = size,
           units ="in",
           ...)
}
