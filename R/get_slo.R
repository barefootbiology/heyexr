get_slo <- function(object) {
    object$slo_image %>%
        as.data.frame %>%
        cbind_rownames("x") %>%
        tbl_df %>%
        gather(y, z, -x) %>%
        mutate(y = str_replace(y, pattern="V", replacement = "")) %>%
        mutate(y = as.numeric(y), x = as.numeric(as.character(x))) %>%
        return()
}
