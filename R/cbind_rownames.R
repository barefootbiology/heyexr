#' @title Add row names as a new column
#'
#' @description Takes the row names and adds them as a new column.
#'
#' @param x data.frame
#' @param new_column column name to add
#'
#' @return data.frame or tbl_df
#'
#' @importFrom magrittr %>%
cbind_rownames <- function(x, column_name="row_names") {
    # If it's a tbl_df, then be sure to return a tbl_df
    # Otherwise return a data.frame
    #return(setNames(cbind(x, row.names(x)), c(names(x), column_name)))
    cbind(x, row.names(x)) %>%
        setNames(c(names(x), column_name)) %>%
        return()
}
