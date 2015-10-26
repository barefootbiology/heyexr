library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(standardlibrary)
library(bit64)


source("R/read_heyex.R")
source("R/read_heyex2.R")

file_path <- "~/stonelab/experiments/oct/oct_atlas/data/laptop-only/2015-09-28/deidentification_test/TEST002.vol"

# Read in a test vol file
#system.time(read_heyex(file_path))
#system.time(read_heyex2(file_path))

#test1 <- read_heyex(file_path)


source("R/read_heyex2.R")
test2 <- read_heyex2(file_path)


# Plot the segmentation curves
data.frame(b_scan = 1,
           seg_layer = c(rep(1,512), rep(2, 512), rep(3, 512)),
           x = rep(1:512, 3),
           y = unlist(test2$seg_array[1:(1*3*512)*3])) %>%
    tbl_df %>%
    ggplot(aes(x = x, y = y)) +
    geom_line(aes(group=as.factor(seg_layer), color=as.factor(seg_layer)))

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
