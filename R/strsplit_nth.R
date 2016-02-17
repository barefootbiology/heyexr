#' Split a string and retain the nth value
#'
#' Retrieve the automated Heidelberg segmentation data from an OCT object
#'
#' @param x a character vector
#' @param split a character string to use as the split pattern
#' @param n either "last" or an integer position
#' @param ... parameters to pass to strsplit
#'
#' @export
#' @return a tbl_df of the segmentation data
strsplit_nth <- function(x, split, n="last", ...) {
    # First, split each character string in the vector
    result <- strsplit(x, split = split, ...)

    # Next, select the nth piece
    result_2 <- lapply(result, function(x) {
        if(n == "last") {
            y <- x[[length(x)]]
        } else {
            y <- x[[as.numeric(n)]]
        }

        return(y)
    })

    # Return a vector of character strings
    return(unlist(result_2))
}
