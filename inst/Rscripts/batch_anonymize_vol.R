#!/usr/bin/env Rscript

library(heyexr)
library(argparser)
library(tidyverse)
library(lubridate)

p <- arg_parser("Anonymize VOL files")

p <- add_argument(p, "--vol",
                  help = "Heidelberg VOL file to parse")

p <- add_argument(p, "--newid",
                  help = "New ID to replace MRN",
                  default = NULL)

# p <- add_argument(p, "--seed",
#                   help = "Seed for non-cryptographic key for image ID.",
#                   default = 0)

p <- add_argument(p, "--outdir",
                  help= "Directory to write results",
                  default = ".")

p <- add_argument(p, "--overwrite",
                  help = "Overwrite existing anonymized file? (y/n)",
                  default = "y")


argv <- parse_args(p)

# Echo the arguments
cat("Parameters:\n")
argv %>% (function(x) data.frame(argument = names(argv), value = unlist(argv)))


overwrite_files <- NULL
if(tolower(argv$overwrite) == "y") {
    overwrite_files = TRUE
} else if(tolower(argv$overwrite) == "n") {
    overwrite_files = FALSE
} else {
    stop("Please specify either y or n for the --overwrite parameter.")
}

# For all the VOL files within a directory, generate SLO images.
# (Capture the nulls from parLapply into a list to prevent them from echoing at the end.)
# temp_list <- parLapply(cl,
#           list.files(argv$indir, full.names = TRUE, pattern="VOL"),
# temp_list <- parLapply(cl,
#           list.files("~/Desktop/oct_controls", full.names = TRUE, pattern="VOL"),
#           render_slo, out_dir = "~/Desktop/oct_controls_slo", draw_margins = FALSE)

# render_oct_summary(vol_file = argv$vol,
#                    xml_file = argv$xml,
#                    center_file = argv$center,
#                    out_dir = argv$outdir,
#                    n_cores = as.numeric(argv$np))

original_vol <- read_vol(argv$vol)

# Construct a new VOL name after the pattern of the original VOL.
# NOTE: Make sure to include the proper underline notation for single B-scan
#       VOL files.

anon_dob <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")

# TASK: Make sure `anonymize_volume` does not write out the original long
#       raw buffer read. (I'm not sure what's in that.)
vol_anon <-
    anonymize_volume(
        volume = original_vol,
        pid = 0L, # TASK: Find out if I need to use this or not.
        patient_id = argv$newid,
        anon_dob = anon_dob
    )

# Compute age at exam
age_at_exam_0 <-
    (vol_anon$header$visit_date - vol_anon$header$dob) / dyears(1)

age_at_exam <- round(age_at_exam_0, 2)

cat(age_at_exam, "\n")

path_out <-
    file.path(
        argv$outdir,
        argv$newid,
        age_at_exam,
        vol_anon$header$scan_position,
        paste0(vol_anon$header$scan_pattern, "_", vol_anon$header$num_bscans)
        )

dir.create(path_out, recursive = TRUE)
dir.create(file.path(path_out, "anonymized"))
dir.create(file.path(path_out, "identified"))

# TASK: Create a directory structure to hold the data.
# <Rosetta ID>/<age_at_visit>/<laterality>/<scan_pattern>_<num_bscans>/anonymized/<Rosetta>_<Hashed imaged ID>.vol
# <Rosetta ID>/<age_at_visit>/<laterality>/<scan_pattern>_<num_bscans>/identified/<ORIGINAL FILENAME>.vol
file_anonymized <-
    paste0(argv$newid, "_", vol_anon$header$id, ".vol")

path_anonymized_file <-
    file.path(path_out, "anonymized", file_anonymized)

write_vol(
    vol_anon,
    path_anonymized_file,
    overwrite = overwrite_files
    )

file.copy(
    from = argv$vol,
    to = file.path(path_out, "identified"),
    overwrite = overwrite_files
    )

cat("Done\n")
