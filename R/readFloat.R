#' Read a float from a binary connection
#'
#' Reads 4 bytes in little Endian and converts to a float
#'
#' @param con a file connection
#'
#' @return a numeric
#'
#' @export
#' @importFrom magrittr %>%
readFloat <- function(con) {
    readBin(con = con, what = "raw",
            size = 1, n = 4,
            endian = "little") %>%
        readBin(what = "numeric",
                size = 4,
                endian = "little") %>%
        return()
}
