library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(standardlibrary)
library(bit64)


file_path <- "~/stonelab/experiments/oct/oct_atlas/data/laptop-only/2015-09-28/deidentification_test/TEST003.vol"

source("R/read_heyex.R")
test <- read_heyex(file_path)


# Plot the segmentation curves
data.frame(b_scan = rep(1:test$header$num_bscans, each = test$header$size_x*test$bscan_header_all[[1]]$num_seg),
           seg_layer = rep(1:test$bscan_header_all[[1]]$num_seg, each = test$header$size_x),
           x = rep(1:test$header$size_x, test$bscan_header_all[[1]]$num_seg),
           y = unlist(test$seg_array)) %>%
    tbl_df %>%
    ggplot(aes(x = x, y = y)) +
    geom_line(aes(group=as.factor(seg_layer), color=as.factor(seg_layer))) +
    facet_grid(b_scan~.)


test_bscan_1 <- matrix(data=test$bscan_images[[18]], nrow=test$header$size_z, byrow=TRUE) %>%
    as.data.frame() %>%
    cbind_rownames("z") %>%
    mutate(z = as.numeric(as.character(z))) %>%
    gather(x, y, -z) %>%
    tbl_df %>%
    mutate(x1 = gsub(as.character(x), pattern="V", replacement=""),
           x = as.numeric(x1),
           y=ifelse(y >= 3.402823e+38, NA, y))


test_bscan_1 %>%
    mutate(y = ifelse(is.na(y), NA, y^0.33)) %>%
    ggplot(aes(x = x, y = z)) +
    geom_tile(aes(fill=y)) +
    scale_fill_continuous(low = "black", high = "white") +
    theme_bw()

ggsave(filename="test_bscan.png", width = 12, height = 8)

# TASK: Plot retinal thickness as heatmap


# # Convert the SLO data to a data.frame compatible with ggplot2
# test_slo <- test$slo_image %>%
#     as.data.frame %>%
#     cbind_rownames("x") %>%
#     tbl_df %>%
#     gather(y, z, -x) %>%
#     mutate(y = str_replace(y, pattern="V", replacement = "")) %>%
#     mutate(y = as.numeric(y), x = as.numeric(as.character(x)))

# Plot the SLO image
# test_slo %>%
#     ggplot(aes(x=x, y=y)) + geom_tile(aes(fill = z)) +
#     theme_bw() +
#     theme(axis.ticks.x = element_blank(),
#           axis.ticks.y = element_blank(),
#           panel.grid = element_blank(),
#           axis.text.x = element_blank(),
#           axis.text.y = element_blank()) +
#     coord_fixed()

# Show all header information except the "spare" slot.
# test$header[1:26]
