#' Parse the b-scan data from an OCT list object
#'
#' Parse the b-scan data from an OCT list object
#'
#' @param oct the OCT list object
#' @param n the ID of the requested b-scan
#'
#' @return a tbl_df containing the b-scan
#'
#' @export
#' @importFrom dplyr tbl_df mutate
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
get_bscan <- function(oct, n=1) {
    matrix(data=oct$bscan_images[[n]], nrow=oct$header$size_z, byrow=TRUE) %>%
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
