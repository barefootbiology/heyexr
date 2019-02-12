#' Read files related to one OCT image
#'
#' Reads a set of files produced by OCTExplorer related to a single OCT image.
#'
#' @export
read_oct_files <- function(volume = NA,
                           segmentation = NA,
                           grid_center = NA,
                           disk_center = NA) {
  result <- list()

  if(is.na(volume) && is.na(segmentation) && is.na(grid_center)) {
    error("Provide at least one file path for volume, segmentation, or grid_center.")
  }

  if(!is.na(volume)) {
    message("Reading volume file ", volume)
    result$volume <- read_vol(vol_file = volume)
  }

  if(!is.na(segmentation)) {
    message("Reading segmentation file ", segmentation)
    result$segmentation <- read_segmentation_xml(xml_file = segmentation)
  }

  if(!is.na(grid_center)) {
    message("Reading grid center file ", grid_center)

    result$grid_center <- read_center_xml(center_file = grid_center)
  }

  if(!is.na(disk_center)) {
    message("Reading disk center file ", disk_center)

    result$disk_center <- read_center_xml(center_file = disk_center)

    # result$disk_center$scan_characteristics$laterality[[1]] <- laterality
  }

  message(length(result), " file(s) successfully read.")
  result
}
