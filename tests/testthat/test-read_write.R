context("test-read_write")

extdata_path <- "inst/extdata"

if(!dir.exists(file.path(here::here(), extdata_path))) {
    extdata_path <- "extdata"
}

original_file <- file.path(path.package("heyexr"), extdata_path, "TEST_T_566581.vol")
original_vol <- read_vol(original_file)

out_file <- tempfile(pattern = "volume")

write_vol(original_vol, out_file, overwrite = TRUE)

written_vol <- read_vol(out_file)

empty_file <- tempfile(pattern = "empty_volume")
file.create(empty_file)

test_that("writing to VOL works", {
  # Skip the version.
  expect_identical(
      original_vol$header[2:length(original_vol$header)],
      written_vol$header[2:length(original_vol$header)]
      )
  expect_identical(
      paste0(original_vol$header$version, "R"),
      written_vol$header$version
  )
  expect_identical(original_vol$slo, written_vol$slo)
  expect_identical(original_vol$seg_array, written_vol$seg_array)
  expect_identical(original_vol$bscan_headers, written_vol$bscan_headers)
  expect_identical(original_vol$bscan_images, written_vol$bscan_images)
})

test_that("file checking works", {
    expect_error(write_vol(original_vol, out_file, overwrite = FALSE))
    expect_error(read_vol("missing.vol"))
    expect_error(read_vol(empty_file))
})

test_that("partial file reading works", {
    expect_identical(
        names(read_vol(original_file, read_what = "header")),
        c("header")
        )
    expect_identical(
        names(read_vol(original_file, read_what = "slo")),
        c("header", "slo_image")
    )
    expect_identical(
        names(read_vol(original_file, read_what = "all")),
        c("header", "slo_image", "bscan_headers", "seg_array", "bscan_images")
    )
})

file.remove(out_file)
close(empty_file)
file.remove(empty_file)
