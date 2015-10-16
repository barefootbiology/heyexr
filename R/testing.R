library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(standardlibrary)
library(bit64)

source("~/stonelab/packages/heyexr/R/read_heyex.R")

# Read in a test vol file
test <- read_heyex("~/stonelab/experiments/oct/oct_atlas/data/laptop-only/2015-09-28/deidentification_test/TEST001.vol")

# Convert the SLO data to a data.frame compatible with ggplot2
test_slo <- test$slo_image %>%
    as.data.frame %>%
    cbind_rownames("x") %>%
    tbl_df %>%
    gather(y, z, -x) %>%
    mutate(y = str_replace(y, pattern="V", replacement = "")) %>%
    mutate(y = as.numeric(y), x = as.numeric(as.character(x)))

# Plot the SLO image
test_slo %>%
    ggplot(aes(x=x, y=y)) + geom_tile(aes(fill = z)) +
    theme_bw() +
    theme(axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    coord_fixed()

# Show all header information except the "spare" slot.
test$header[1:26]
