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
    num_seg <- unlist(oct$bscan_header[1, "num_seg"])

    data.frame(b_scan = rep(1:oct$header$num_bscans,
                            each = oct$header$size_x*num_seg),
               seg_layer = rep(1:num_seg, each = oct$header$size_x),
               x = rep(1:oct$header$size_x, num_seg),
               z = unlist(oct$seg_array)) %>%
        tbl_df %>%
        return()
}
