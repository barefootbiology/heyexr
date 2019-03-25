# 'Pad a raw vector with null bytes
#'
#' \code{pad_raw} adds null bytes to the end of a raw vector to give a vector of
#' a specified size.
#'
#' @param x A raw vector.
#' @param n The total number of elements in the output vector.
#' @param size The number of bytes taken up by each element.
#'
#' @return A raw vector of length n.
pad_raw <- function(x, n, size = 1L) {
    out <- raw(n * size)

    n_bytes <- length(x) * size

    if(n_bytes > 0) {
        out[1:n_bytes] <- x
    }

    out
}
