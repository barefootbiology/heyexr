#' Convert a list to a single-row tibble.
#'
#' \code{list_to_onerow} converts lists with multiple levels to a data frame of
#'  a single row.
#'
#' @param x A list.
#'
#' @return A tibble.
#'
#' @examples
#' mylist <- list("A" = 1, "B" = c(1,2,4),
#'                "C" = letters[1:5],
#'                "D" = list(10:20, fruit = c("orange", "apple", "banana")))
#' mylist
#' list_to_onerow(mylist)
#' @importFrom tibble as_tibble
#' @export
list_to_onerow <- function(x) {
    onelevel <- promote_elements(x, recursive = TRUE)
    # TASK: For final output, warn if there are duplicated column names.
    as_tibble(onelevel)
}
