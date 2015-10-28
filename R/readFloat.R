readFloat <- function(con) {
    readBin(con = con, what = "raw",
            size = 1, n = 4,
            endian = "little") %>%
        readBin(what = "numeric",
                size = 4,
                endian = "little") %>%
        return()
}
