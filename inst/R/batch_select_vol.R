# Parse header information and sort VOL files.
# Usage:
# Rscript inst/R/batch_select_vol.R --dir /Users/scottwhitmore/stonelab/experiments/oct/oct_crb1_rs1_controls/data/vol --outdir ~/Desktop --filter 3 --results ~/Desktop/vol_sort_testing.txt

library(heyexr)
library(argparser)
library(tidyverse)

p <- arg_parser("Render OCT summaries in parallel")

p <- add_argument(p, "--dir",
                  help = "Directory containing VOL files to investigate")

p <- add_argument(p, "--outdir",
                  help = "Directory to copy VOL files of interest",
                  default = NULL)

p <- add_argument(p, "--filter",
                  help = "Term to filter VOL files",
                  default = NULL)

p <- add_argument(p, "--results",
                  help="Name of text file to save the header information",
                  default = NULL)

argv <- parse_args(p)


# List all the VOL files in the directory of interest
vol_file_list <- data.frame(file = unlist(list.files(path = argv$dir,
                            pattern = "vol$",
                            ignore.case = TRUE,
                            full.names = TRUE))) %>%
    mutate(file = as.character(file))

vol_file_list

# Wrapper for reading the VOL header information
read_heyex_header_only <- function(vol_file) {
    file_con <- file(vol_file, "rb")

    vol_header <- read_heyex_header(vol_con = file_con)

    close(file_con)

    # We don't need the "spare" data at the end of the header
    return(vol_header[c(1:14, 16:21,23:26)])
}

# Return a list of lists
all_headers_list <- vol_file_list %>%
    split(.$file) %>%
    map(~read_heyex_header_only(vol_file = .$file))

# Merge as a single data.frame
all_headers <- all_headers_list %>%
    lapply(data.frame) %>%
    bind_rows(.id = "file")

# Save output to the specified text file
all_headers %>%
    write_tsv(path = argv$results)

# Copy specific vol files to a location
if(!is.null(argv$filter)) {
    all_headers %>%
        filter(as.character(scan_pattern) == argv$filter) %>%
        split(.$file) %>%
        walk(~file.copy(from = .$file, to = argv$outdir))
}







