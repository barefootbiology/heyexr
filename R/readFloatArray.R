readFloatArray <- function(con, n = 1) {
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
