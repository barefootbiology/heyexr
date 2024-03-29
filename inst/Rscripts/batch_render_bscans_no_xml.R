#!/usr/bin/env Rscript

# Parallelization based on:
# http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

# library(parallel)
library(heyexr)
library(argparser)
library(dplyr)

p <- arg_parser("Render OCT summaries in parallel")

p <- add_argument(p, "--vol",
                  help = "Heidelberg VOL file to parse")

p <- add_argument(p, "--xml",
                  help = "XML segmentation from OCT Explorer",
                  default = NULL)

p <- add_argument(p, "--center",
                  help="XML file specifying the grid center",
                  default = NULL)

p <- add_argument(p, "--np",
                  help="Number of cores to use",
                  default = 1)

# p <- add_argument(p, "--indir",
#                   help="Directory containing VOL files",
#                   default = ".")

p <- add_argument(p, "--outdir",
                  help="Directory to write results",
                  default = ".")

p <- add_argument(p, "--slo",
                  help="Render just the SLO and ignore the b-scans?",
                  default = 0)


argv <- parse_args(p)

# Echo the arguments
cat("Parameters:\n")
argv %>% (function(x) data.frame(argument = names(argv), value = unlist(argv)))


# For all the VOL files within a directory, generate SLO images.
# (Capture the nulls from parLapply into a list to prevent them from echoing at the end.)
# temp_list <- parLapply(cl,
#           list.files(argv$indir, full.names = TRUE, pattern="VOL"),
# temp_list <- parLapply(cl,
#           list.files("~/Desktop/oct_controls", full.names = TRUE, pattern="VOL"),
#           render_slo, out_dir = "~/Desktop/oct_controls_slo", draw_margins = FALSE)

# TESTING: I need to remove the hard coding of NULL for these values here.
render_oct_summary(vol_file = argv$vol,
                   xml_file = NULL,
                   center_file = NULL,
                   out_dir = argv$outdir,
                   crop_to_heidelberg_segmentation = NULL,
                   file_type = "png",
                   slo_only = ifelse(argv$slo == 0, FALSE, TRUE))

cat("Done\n")
