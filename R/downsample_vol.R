#' Downsample B-scans in a volume object.
#'
#' \code{downsample_vol} reduces the number of B-scans in a volume
#' object. The reduction maintains the central B-scan, and ensures that
#' the distance between B-scans is constant.
#'
#' @param vol A volume object.
#' @param new_n The number of B-scans to retain.
#'
#' @return A volume object.
#' @export
#' @importFrom magrittr %>%
downsample_vol <- function(vol, new_n) {

  vol_down <- vol

  down_mapping <- downsample_mapping(vol$header$num_bscans, new_n)

  vol_down$header$distance <- vol_down$header$distance * down_mapping$factor

  vol_down$header$num_bscans <- as.integer(new_n)

  vol_down$bscan_headers <-
    vol_down$bscan_headers %>%
    filter(bscan_id %in% down_mapping$bscan_id) %>%
    mutate(bscan_id = 1:n())

  vol_down$seg_array <- vol_down$seg_array[,down_mapping$bscan_id,]

  vol_down$bscan_images <- vol_down$bscan_images[,down_mapping$bscan_id,]

  vol_down
}
