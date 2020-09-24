context("test-read_write")

extdata_path <- "extdata"

original_file <- file.path(path.package("heyexr"), extdata_path, "TEST_T_566581_Surfaces_Iowa.xml")

if(!file.exists(original_file)) {
    extdata_path <- "inst/extdata"
    original_file <- file.path(here::here(), extdata_path, "TEST_T_566581_Surfaces_Iowa.xml")
}

original_xml <- read_segmentation_xml(original_file)

out_file <- tempfile(pattern = "volume")

# write_vol(original_xml, out_file, overwrite = TRUE)

# written_xml <- read_vol(out_file)

# empty_file <- tempfile(pattern = "empty_volume")
# file.create(empty_file)

test_that("filtering segmentation XML works", {
    expect_warning(filter_segmentation_xml(original_file, c(10, 10)))
    expect_error(filter_segmentation_xml(original_file, c(-10)))

    # TASK: Test files are identical if using default surface IDs.

    # TASK: Test that files have reduced number of surfaces
})
