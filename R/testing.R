library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(standardlibrary)
library(bit64)


file_path <- "~/stonelab/experiments/oct/oct_atlas/data/laptop-only/2015-09-28/deidentification_test/TEST001.vol"

source("R/read_heyex.R")
test <- read_heyex(file_path)


# Plot the segmentation curves
data.frame(b_scan = rep(1:19, each = 512*3),
           seg_layer = c(rep(1,512), rep(2, 512), rep(3, 512)),
           x = rep(1:512, 3),
           y = unlist(test$seg_array)) %>%
    tbl_df %>%
    ggplot(aes(x = x, y = y)) +
    geom_line(aes(group=as.factor(seg_layer), color=as.factor(seg_layer))) +
    facet_grid(b_scan~.)

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
