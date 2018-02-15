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
get_segmentation <- function(oct) {
    oct$seg_array %>%
        reshape2::melt() %>%
        as_tibble() %>%
        setNames(c("x", "b_scan", "seg_layer", "z")) %>%
        inner_join(tibble(seg_layer = 1:3,
                          surface = c("ILM", "RPE", "NFL")) %>%
                       mutate(surface = factor(surface,
                                               levels = c("ILM", "RPE", "NFL"))))
}
