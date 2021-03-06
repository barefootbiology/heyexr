#' Import the CSV produced by "Thickness Analysis" in OCT Explorer
#'
#' Imports and formats the CSV data produced by the "Thickness Analysis"
#' performed by OCT Explorer software. OCT Explorer is currently distributed for
#' Windows, and the "Surfaces" column contains the full path to each XML
#' segmentation file, producing unwieldy identifiers. Therefore, handy default
#' values for the parameters split, n, and pattern are provided for cleaning up
#' the values in the Surfaces column.
#'
#' @param csv_file the CSV file to import
#' @param scrub a string to remove from the file column generated by OCT Explorer
#' @param split a character string to use as the split pattern
#' @param n either "last" or an integer position
#' @param pattern character string to remove from the values in the Surfaces column
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select rowwise rename ungroup
#' @importFrom tidyr gather separate spread
#' @importFrom stringr str_replace_all
#' @importFrom readr read_csv
import_thickness_csv_new <- function(csv_file,
                                 split="\\\\",
                                 n="last",
                                 pattern="_Surfaces_Iowa.xml") {
    result <- read_csv(csv_file) %>%
        (function(x) x %>% select(-ncol(x))) %>%
        gather(key=key, value=um, matches("Thickness")) %>%
        mutate(key_stat=ifelse(grepl(key, pattern="Mean"), "mean","sd"),
                      key=gsub(key,
                               pattern="^MeanThickness_|^SDThickness_",
                               replacement="", perl = TRUE))

    # Remove the "%" symbol from any of the column names!
    result <- setNames(result, gsub(names(result), pattern="\\%",
                                    replacement = "percent", perl=TRUE))

    # Get the means
    result_mean <- result %>%
        filter(key_stat == "mean") %>%
        spread(key_stat, um)

    # Get the standard deviations
    result_sd <- result %>%
        filter(key_stat == "sd") %>%
        spread(key_stat, um)

    # 1. Combine the means and standard deviations.
    # 2. Rename the columns
    # 3. Add a sample ID by cleaning up the surfaces values
    result <- inner_join(result_mean, result_sd) %>%
        rename(surfaces = Surfaces,
               laterality = Laterality,
               oct_center_type = OCTCenterType,
               oct_size_x_voxel = OCTSizeX_voxel,
               oct_size_y_voxel = OCTSizeY_voxel,
               oct_size_z_voxel = OCTSizeZ_voxel,
               physical_size_x_mm = PhysicalSizeX_mm,
               physical_size_y_mm = PhysicalSizeY_mm,
               physical_size_z_mm = PhysicalSizeZ_mm,
               voxel_size_x_um = VoxelSizeX_um,
               voxel_size_y_um = VoxelSizeY_um,
               voxel_size_z_um = VoxelSizeZ_um,
               grid = Grid,
               grid_center = GridCenter,
               grid_center_x_pixel = GridCenterX_pixel,
               grid_center_y_pixel = GridCenterY_pixel,
               undefined_region_percent = UndefinedRegion_percent) %>%
        mutate(sample_id = as.character(surfaces)) %>%
        #rowwise() %>%
        mutate(sample_id = strsplit_nth(sample_id,
                                       split = split,
                                       n = n,
                                       perl=TRUE)) %>%
        #ungroup %>%
        mutate(sample_id = gsub(sample_id, pattern = pattern, replacement = ""))

    result <- result %>%
        mutate(key = gsub(key, pattern="_um$", replacement="", perl = TRUE)) %>%
        separate(key, c("span","region"), sep="_Region", remove=TRUE) %>%
        separate(span, c("inner_layer","outer_layer"), sep="_To_", remove=TRUE) %>%
        mutate(region = as.character(region))
#         inner_join(layer_map) %>%
#         select(layer, etdrs_region, um, sample_id)

    return(result)
}
