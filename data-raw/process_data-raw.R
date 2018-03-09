# Process data-raw
library(dplyr)
library(readr)

grid_regions <- read.table("data-raw/grid_regions.txt",
           sep = "\t", header = TRUE) %>%
    tbl_df %>%
    mutate(laterality = as.character(laterality)) %>%
    mutate(region = as.character(region)) %>%
    mutate(flipped_laterality = as.character(flipped_laterality)) %>%
    mutate(flipped_region = as.character(flipped_region))

devtools::use_data(grid_regions, overwrite = TRUE)



readxl::read_excel(path = "data-raw/layer_definitions.xlsx") %>%
    write_tsv("data-raw/layer_definitions_iowa_v3-8-0.tsv")

layer_data <- read_tsv(file = "data-raw/layer_definitions_iowa_v3-8-0.tsv") %>%
    mutate(layer = factor(layer, levels = layer)) %>%
    mutate(layer_description = factor(layer_description, levels = layer_description))

devtools::use_data(layer_data, overwrite = TRUE)

# TASK: Construct this automatically from the original OCTExplorer definition.
grid_sectors_etdrs <- read_tsv(file = "data-raw/grid_sectors_etdrs.tsv")

devtools::use_data(grid_sectors_etdrs, overwrite = TRUE)
