#' Split a string and retain the nth value
#'
#' Retrieve the automated Heidelberg segmentation data from an OCT object
#'
#' @param x a character string
#' @param split a character string to use as the split pattern
#' @param n either "last" or an integer position
#' @param ... parameters to pass to strsplit
#'
#' @return a tbl_df of the segmentation data
strsplit_nth <- function(x, split, n="last", ...) {
    # TASK: Use an apply function to vectorize this procedure
    result <- strsplit(x, split = split, ...)
    result <- unlist(result)

    if(n == "last") {
        result <- result[[length(result)]]
    } else {
        result <- result[[as.numeric(n)]]
    }

    return(result)
}
