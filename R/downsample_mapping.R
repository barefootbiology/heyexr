#' Downsample a sequence of numbers.
#'
#' \code{downsample_mapping} downsamples a sequence of numbers, 1 through
#' \code{n} and returns a vector of indices, centered on the middle
#' number of the original sequence.
#'
#' @param original_n An integer giving the length of the original sequence of
#'     numbers.
#' @param down_n The desired size of the downsampled sequence.
#'
#' @return A list containing the downsampling factor and a
#'     vector a downsampled indices.
#'
#' @export
#' @importFrom magrittr %>%
downsample_mapping <- function(original_n, down_n) {
  if(original_n <= down_n) {
    stop("Downsampling number is greater than or equal to original number!\t")
  }

  # Center downsampling on the middle b-scan
  # TASK: Consider what to do in the rare case of a non-odd number.
  original_center <-  stats::median(1:original_n)

  down_center <- stats::median(1:down_n)

  down_factor <- floor(original_n / down_n)

  half_length <- ceiling(down_n / 2)

  lower_seq <-
    seq(
      from = original_center,
      by = -1 * down_factor,
      length.out = half_length
    )

  upper_seq <-
    seq(
      from = original_center,
      by = down_factor,
      length.out = half_length
    )

  down_seq <-
    c(lower_seq, upper_seq) %>%
    unique() %>%
    sort() %>%
    as.integer()

  # TASK: Better would be to return a vector with the down_factor as
  #       an attribute.
  list(factor = down_factor, bscan_id = down_seq)
}
