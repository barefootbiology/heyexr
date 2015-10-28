get_bscan <- function(object, n=1) {
    matrix(data=object$bscan_images[[n]], nrow=object$header$size_z, byrow=TRUE) %>%
        as.data.frame() %>%
        cbind_rownames("z") %>%
        mutate(z = as.numeric(as.character(z))) %>%
        gather(x, intensity, -z) %>%
        tbl_df %>%
        mutate(x = as.numeric(gsub(as.character(x), pattern="V", replacement="")),
               intensity = unlist(intensity),
               intensity=ifelse(intensity >= 3.402823e+38, NA, intensity)) %>%
        return
}
