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

    # readBin(con = con, what = "numeric", n = n)

    by(raw_floats, rep(1:n, each=4), (function(x) readBin(x, what = "numeric",
                                                          size = 4,
                                                          endian = "little")),
       simplify = TRUE) %>%
        as.numeric() %>%
        return()

    # # Attempt 2 - still slow
    # raw_floats %>%
    #     split(rep(1:n, each = 4)) %>%
    #     map_dbl(~readBin(.x, what = "numeric", size = 4, endian = "little")) %>%
    #     return

    # # Attempt 3 - Split is rather slow when used with LOTS of factor levels.
    # #             This is my attempt to bypass using split.
    # result <- list()
    #
    # for(i in c(1:n)) {
    #     result[[i]] <- readBin(raw_floats[(i - 1) * 4 + c(1:4)],
    #                          what = "numeric", size = 4, endian = "little")
    # }
    #
    # # Return the result as a vector of numeric values
    # return(unlist(result))
    # # CONCLUSION -- REALLY slow.

    # # Let's try this with Rcpp
    # cppFunction('NumericVector asFloatVector(RawVector rx, int n)  {
    #     NumericVector out(n);
    #
    #     for(int i = 0; i < n; ++i) {
    #         out[i] <- readBin(raw_floats[(i - 1) * 4 + c(1:4)],
    #                              what = "numeric", size = 4, endian = "little")
    #     }
    #
    # }')


}
