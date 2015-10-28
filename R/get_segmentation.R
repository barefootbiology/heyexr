get_segmentation <- function(object) {
    data.frame(b_scan = rep(1:object$header$num_bscans, each = object$header$size_x*object$bscan_header[[1]]$num_seg),
               seg_layer = rep(1:object$bscan_header[[1]]$num_seg, each = object$header$size_x),
               x = rep(1:object$header$size_x, object$bscan_header[[1]]$num_seg),
               y = unlist(object$seg_array)) %>%
        tbl_df %>%
        return()
}
