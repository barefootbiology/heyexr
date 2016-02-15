#' Parse the b-scan data from an OCT list object
#'
#' Parse the b-scan data from an OCT list object
#'
#' @param object the OCT list object
#' @param n the ID of the requested b-scan
#'
#' @return a tbl_df containing the b-scan
#'
#' @export
#' @importFrom dplyr tbl_df mutate
#' @importFrom tidyr gather
#' @importFrom standardlibrary cbind_rownames
#' @importFrom magrittr %>%
get_bscan <- function(object, n=1) {
    matrix(data=object$bscan_images[[n]], nrow=object$header$size_z, byrow=TRUE) %>%
        as.data.frame() %>%
        cbind_rownames("z") %>%
        mutate(z = as.numeric(as.character(z))) %>%
        gather(x, intensity, -z) %>%
        tbl_df %>%
        mutate(x = as.numeric(gsub(as.character(x), pattern="V", replacement="")),
               intensity = unlist(intensity),
               intensity=ifelse(intensity >= 3.402823e+38, NA, intensity)) %>%
        return
}
