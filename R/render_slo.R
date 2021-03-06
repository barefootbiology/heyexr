#' Render SLO data
#'
#' Reads SLO data from a VOL file and renders as an image in the specified
#' format.
#'
#' @param file a VOL file
#' @param out_dir directory to save image
#' @param type image format (device)
#' @param size x/y dimensions in units
#' @param units units for x/y dimensions
#'
#' @return none
#'
#' @export
#' @importFrom magrittr %>%
render_slo <- function(vol_file, out_dir=".", draw_margins = FALSE, type="png",
                       size = 4, units ="in", ...) {
    # For a give VOL file, read the SLO information.
    oct <- read_heyex(vol_file, header_slo_only=TRUE)

    p_slo <- construct_slo(oct, draw_margins = draw_margins)

    # Extract the basename from the VOL file
    file_base = basename(vol_file) %>%
        gsub(pattern = ".vol$", replacement = "",
             perl = TRUE, ignore.case = TRUE)

    if(!dir.exists(out_dir)) {
        dir.create(out_dir)
    }

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
