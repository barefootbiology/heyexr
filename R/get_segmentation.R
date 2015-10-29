get_segmentation <- function(object) {
    num_seg <- unlist(object$bscan_header[1, "num_seg"])

    data.frame(b_scan = rep(1:object$header$num_bscans,
                            each = object$header$size_x*num_seg),
               seg_layer = rep(1:num_seg, each = object$header$size_x),
               x = rep(1:object$header$size_x, num_seg),
               y = unlist(object$seg_array)) %>%
        tbl_df %>%
        return()
}
