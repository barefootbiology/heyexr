#' Get the segmentation data from an OCT object
#'
#' Retrieve the automated Heidelberg segmentation data from an OCT object
#'
#' @param oct OCT list object
#'
#' @return a tbl_df of the segmentation data
#'
#' @export
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
#' @importFrom rlang set_names .data
#' @importFrom reshape2 melt
get_segmentation <- function(oct) {
    oct$seg_array %>%
        # TASK: Replace the following 3 lines with the "melt_array" function.
        reshape2::melt() %>%
        as_tibble() %>%
        set_names(c("x", "bscan_id", "surface_id", "z")) %>%
        inner_join(tibble(surface_id = 1:3,
                          layer = c("ILM", "RPE", "NFL")) %>%
                       mutate(layer = factor(.data$layer,
                                               levels = c("ILM", "RPE", "NFL"))))
}
