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
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
get_slo <- function(oct) {
    oct$slo_image %>%
        as.data.frame %>%
        rownames_to_column("x") %>%
        tbl_df %>%
        gather("y", "z", -.data$x) %>%
        mutate(y = stringr::str_replace(.data$y, pattern="V", replacement = "")) %>%
        mutate(y = as.numeric(.data$y), x = as.numeric(as.character(.data$x)))
}
