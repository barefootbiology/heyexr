#' Correct contrast
#'
#' Adjust the intensity values using a spline model
#' based on the difference contrast correction performed
#' by Heidelberg Explorer.
#'
#' @return A vector of transformed intensities
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr if_else
spline_correction <- function(i) {
    pi <- predict(vol_fit_spline, x = i)$y
    if_else(pi < 0, 0, pi) %>%
        return()
}
