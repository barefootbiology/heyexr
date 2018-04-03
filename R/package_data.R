#' Corresponding grid regions for right (OD) and left (OS) eyes.
#'
#' A dataset containing a mapping between grid regions in OS and OD eyes.
#'
#' @format A tbl_df with 9 rows and 3 variables:
#' \describe{
#'   \item{grid}{grid, the underlying XML file defining the grid used by OCT Explorer to measure thickness}
#'   \item{laterality}{laterality, anatomic position of the eye}
#'   \item{region}{region, region in the grid used to measure thickness}
#'   \item{flipped_laterality}{flipped_laterality, transformed anatomic position of the eye}
#'   \item{flipped_region}{flipped_region, transformed region in the grid used to measure thickness}
#' }
"grid_regions"


#' A spline fit to the PNG exported from Heidelberg Explorer.
#'
#' A smooth.spline object.
#'
#' @format A smooth.spline object.
"vol_fit_spline"

#' Value used to indicate missing data (NA).
#'
#' Maximum floating point value used by Heidelberg Engineering to represent
#' missing data.
#'
#' @format A scalar double.
"max_float"

#' Metadata on retinal layers identified by the Iowa Reference Algorithms
#'
#' Metadata on segmentation surfaces and layers identified by the Iowa
#' Reference Algorithms (v 3.8.0).
#'
#' @format A tibble
"layer_info"
