#' Read an vector of floats from a binary connection
#'
#' Reads n number of 4 bytes in little Endian and converts numeric vector
#'
#' @param con a file connection
#' @param n size of vector
#'
#' @return a numeric
#'
#' @export
#' @importFrom magrittr %>%
read_float_vector <- function(con, n = 1) {
    raw_floats <- readBin(con = con, what = "raw",
                          size = 1, n = 4*n,
                          endian = "little")

    by(raw_floats, rep(1:n, each=4), (function(x) readBin(x, what = "numeric",
                                                          size = 4,
                                                          endian = "little")),
       simplify = TRUE) %>%
        as.numeric() %>%
        return()
}
