#' Convert a list to a wide tibble.
#'
#' \code{promote_elements} converts multi-value elements of a list into uniquely
#' named elements of the [primary] list. [What's a better way to say this?]
#'
#' @param x A list.
#'
#' @return If any of the elements of the list had multiple value, these
#'   values will now be included as separate elements in the top level.
#'
#' @examples
#'
#' mylist <- list("A" = 1, "B" = c(1,2,4),
#'                "C" = letters[1:5],
#'                "D" = list(10:20, fruit = c("orange", "apple", "banana")))
#' mylist
#' promote_elements(mylist)
#'
#' # The following two lines are equivalent for this example:
#' promote_elements(promote_elements(mylist))
#' promote_elements(mylist, recursive = TRUE)
#' @importFrom purrr map
#' @export
promote_elements <- function(x, combine_names = TRUE, recursive = FALSE) {
    x_names <- names(x)

    result <- list()

    for (name in x_names) {
        new_element <- x[[name]]

        if(length(x[[name]]) > 1) {

            element_names <- names(new_element)

            new_element_names <-

                if(is.null(element_names)) {
                    new_element_names <- as.character(1:length(new_element))
                } else {
                    new_element_names <- if_else(element_names == "", as.character(1:length(new_element)), element_names)
                }

            new_element <- setNames(x[[name]], paste(name, new_element_names, sep = "_")) %>%
                as.list()

            result <- append(result, new_element)
        } else {
            result[[name]] <- new_element
        }
    }

    if(recursive) {
        element_lengths <- purrr::map(result, length)

        if(any(element_lengths > 1)) {
            return(promote_elements(x = result, combine_names = combine_names, recursive = recursive))
        } else {
            return(result)
        }
    }

    result
}
