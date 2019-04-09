#' Remove singleton lists of a list.
#'
#' \code{delist_singletons} searches a list for elements that are list of length
#' 1 and "delists" by assigning the contents to the name of the list.
#'
#' @param x A list.
#'
#' @return A list.
#' @examples
#' mylist <- list("A" = 1, "B" = list(1:5), "C" = list(little = 1:3, tiny = 4:5))
#' mylist
#' delist_singletons(mylist)
#' @importFrom purrr map
#' @export
delist_singletons <- function(x) {
    x_lengths <- map(.x = x, length)
    x_classes <- map(.x = x, class)

    indices <- which((x_classes == "list") & (x_lengths == 1))

    for(i in indices) {
        x[[i]] <- x[[i]][[1]]
    }

    x
}
