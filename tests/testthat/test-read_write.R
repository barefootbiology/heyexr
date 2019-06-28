context("test-read_write")

extdata_path <- "extdata"

original_file <- file.path(path.package("heyexr"), extdata_path, "TEST_T_566581.vol")

if(!file.exists(original_file)) {
    extdata_path <- "inst/extdata"
    original_file <- file.path(here::here(), extdata_path, "TEST_T_566581.vol")
}


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
      written_vol$header[2:length(written_vol$header)]
      )
  expect_identical(
      original_vol$header$version,
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

# anon_dob <- as.POSIXct(0, origin = "1954-09-29", tz = "UTC")    # WORKS
# anon_dob <- as.POSIXct(0, origin = "1954-09-28", tz = "UTC")    # FAILS

anon_dob <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")

vol_anon <-
    anonymize_volume(
        volume = original_vol,
        pid = 123L,
        patient_id = "ABC456",
        anon_dob = anon_dob
        )

test_that("identifying information is changed in the volume object", {
    expect_false(original_vol$header$exam_time == vol_anon$header$exam_time)
    expect_false(original_vol$header$visit_date == vol_anon$header$visit_date)
    expect_false(original_vol$header$dob == vol_anon$header$dob)
    expect_false(original_vol$header$pid == vol_anon$header$pid)
    expect_false(original_vol$header$patient_id == vol_anon$header$patient_id)
})

anon_file <- tempfile(pattern = "volume_anon")

write_vol(vol_anon, anon_file, TRUE)

vol_anon_in <- read_vol(anon_file)

test_that("anonymized information remains the same before and after writing to a file", {
    expect_equal(vol_anon$header$exam_time, vol_anon_in$header$exam_time)
    expect_equal(vol_anon$header$visit_date, vol_anon_in$header$visit_date)
})

test_that("identifying information is changed once written to a VOL file", {
    expect_identical(vol_anon_in$header$dob, anon_dob)
    expect_equal(
        vol_anon_in$header$visit_date - vol_anon_in$header$dob,
        original_vol$header$visit_date - original_vol$header$dob
        )
    expect_equal(
        vol_anon_in$header$exam_time - vol_anon_in$header$dob,
        original_vol$header$exam_time - original_vol$header$dob
    )
    expect_identical(vol_anon_in$header$pid, 123L)
    expect_identical(vol_anon_in$header$patient_id, "ABC456")
})

file.remove(out_file)
file.remove(empty_file)
file.remove(anon_file)
