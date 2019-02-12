#' Add a column specifying if the values in the segmentation are undefined
#'
#' Adds a column `is_defined` which indicates if a particular A-scan could be
#' reliably segmented. NOTE: This function assumes there is a column named
#' "value", but this column  name is likely to change in future iterations of
#' the software.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr full_join mutate if_else
add_undefined <- function(data, undefined) {
  na_value <- as.numeric(NA)

  if(is.integer(data$value)) {
    na_value <- as.integer(NA)
  }

  data %>%
    full_join(undefined) %>%
    mutate(is_defined = if_else(is.na(is_defined),
                                TRUE, is_defined)) %>%
    mutate(value_defined =
             if_else(is_defined,
                     value,
                     na_value))
}

