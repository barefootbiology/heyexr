render_slo <- function(file, out_dir=".", type="png", size = 4, units ="in", ...) {
    # For a give VOL file, read the SLO information.
    oct <- read_heyex(file, header_slo_only=TRUE)

    p_slo <- construct_slo(oct)

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
