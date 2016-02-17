# Process data-raw
library(dplyr)

grid_regions <- read.table("data-raw/grid_regions.txt",
           sep = "\t", header = TRUE) %>%
    tbl_df %>%
    mutate(laterality = as.character(laterality)) %>%
    mutate(region = as.character(region)) %>%
    mutate(flipped_laterality = as.character(flipped_laterality)) %>%
    mutate(flipped_region = as.character(flipped_region))

devtools::use_data(grid_regions, overwrite = TRUE)
