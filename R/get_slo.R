#' Get the SLO data from an OCT object
#'
#' Retrieves the SLO data from an OCT object
#'
#' @param oct an OCT list oct
#'
#' @return a tbl_df of the SLO data
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom stringr str_replace
get_slo <- function(oct) {
    oct$slo_image %>%
        as.data.frame %>%
        cbind_rownames("x") %>%
        tbl_df %>%
        gather(y, z, -x) %>%
        mutate(y = stringr::str_replace(y, pattern="V", replacement = "")) %>%
        mutate(y = as.numeric(y), x = as.numeric(as.character(x))) %>%
        return()
}
