#' Read OCT volume, angiography, segmentation, and grid center.
#'
#' Read OCT volume, angiography, segmentation, and grid center.
#'
#' @return A list of objects.
#'
#' @export
read_ssoct_files <- function(volume = NA,
                             angio = NA,
                             segmentation = NA,
                             grid_center = NA,
                             disk_center = NA,
                             laterality = NA) {

    result <- list()

    if(is.na(volume) &&
       is.na(angio) &&
       is.na(segmentation) &&
       is.na(grid_center) &&
       is.na(disk_center)) {

        error("Provide at least one file path for volume, segmentation, or grid_center.")

    }

    if(!is.na(volume)) {
        message("Reading volume file ", volume)

        result$volume <- read_nifti(nifti_file = volume)

        result$volume$header$scan_position <- laterality
    }

    if(!is.na(angio)) {
        message("Reading angiography file ", angio)

        result$angio <- read_nifti(nifti_file = angio)

        result$angio$header$scan_position <- laterality

    }

    if(!is.na(segmentation)) {
        message("Reading segmentation file ", segmentation)

        result$segmentation <- read_segmentation_xml(xml_file = segmentation)
    }

    if(!is.na(grid_center)) {
        message("Reading grid center file ", grid_center)

        result$grid_center <- read_center_xml(center_file = grid_center)

        result$grid_center$scan_characteristics$laterality[[1]] <- laterality
    }

    if(!is.na(disk_center)) {
        message("Reading disk center file ", disk_center)

        result$disk_center <- read_center_xml(center_file = disk_center)

        result$disk_center$scan_characteristics$laterality[[1]] <- laterality
    }

    message(length(result), " file(s) successfully read.")

    result
}
