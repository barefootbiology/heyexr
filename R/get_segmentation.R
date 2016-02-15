#' Get the segmentation data from an OCT object
#'
#' Retrieve the automated Heidelberg segmentation data from an OCT object.
#'
#' @param object OCT list object
#'
#' @return a tbl_df of the segmentation data
#'
#' @export
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
get_segmentation <- function(object) {
    num_seg <- unlist(object$bscan_header[1, "num_seg"])

    data.frame(b_scan = rep(1:object$header$num_bscans,
                            each = object$header$size_x*num_seg),
               seg_layer = rep(1:num_seg, each = object$header$size_x),
               x = rep(1:object$header$size_x, num_seg),
               z = unlist(object$seg_array)) %>%
        tbl_df %>%
        return()
}
