#' Flip grid region for a table of thickness values
#'
#' Ensures that all thickness values are comparable across regions
#'
#' @param x a data.frame of thickness values. Must contain 'grid', 'laterality', and 'region' columns.
#' @param flip_to laterality to enforce for all measurements
#'
#' @return a tbl_df of the original data plus two new columns, 'flipped_laterality' and 'flipped_region'
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join filter
#' @importFrom rlang .data
flip_region <- function(x, flip_to="OD") {
    # flip_to must either by "OD" or "OS".
    # Make sure to ignore the case

    # NOTE: This line added to address CRAN check NOTE:
    #       "no visible binding for global variable ‘grid_regions’"
    grid_regions <- get("grid_regions")

    result <- x %>%
        inner_join(filter(grid_regions,
                          .data$flipped_laterality == toupper(flip_to)))

    # TASK: Make this error check better.
    if(nrow(result) == nrow(x)) {
       return(result)
    } else {
        return("ERROR: The original number of rows in x does not equal the new number of rows in the result!")
    }

}
