#!/usr/bin/env Rscript
#!/Shared/IVR/whitmores/bin/Rscript

# Parallelization based on:
# http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

library(parallel)
library(heyexr)
library(argparser)

p <- arg_parser("Render OCT summaries in parallel")

p <- add_argument(p, "--np",
                  help="Number of cores to use",
                  default = 3)

p <- add_argument(p, "--indir",
                  help="Directory containing VOL files",
                  default = ".")

p <- add_argument(p, "--outdir",
                  help="Directory to write results",
                  default = ".")

argv <- parse_args(p)


no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "FORK")
clusterEvalQ(cl, library(heyexr))

# For all the VOL files within a directory, generate SLO images.
# (Capture the nulls from parLapply into a list to prevent them from echoing at the end.)
temp_list <- parLapply(cl,
          list.files(argv$indir, full.names = TRUE, pattern="VOL"),
          render_oct_summary, out_dir = argv$outdir)

# temp_list <- parLapply(cl,
#           list.files("~/Desktop/oct_controls", full.names = TRUE, pattern="VOL"),
#           render_slo, out_dir = "~/Desktop/oct_controls_slo", draw_margins = FALSE)


stopCluster(cl)
