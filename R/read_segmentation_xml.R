#' Read layer segmentation output from Iowa Reference Algorithms
#'
#' Reads the layer segmentation as output by the Iowa Reference Algorithms (XML format).
#'
#' @param xml_file path to the segmentation XML file
#'
#' @return a list containing the header information and the layer segmentation
#'
#' @export
#' @importFrom dplyr tbl_df filter rename mutate mutate group_by ungroup select distinct inner_join bind_rows
#' @importFrom magrittr %>%
#' @importFrom xml2 read_xml xml_find_first xml_find_all xml_text xml_integer xml_double
read_segmentation_xml <- function(xml_file) {

    oct_file_parsed_tab <- gsub(xml_file, pattern="\\.xml$",
                                replacement=".txt", perl = TRUE)

    # Loading the XML version
    # oct_surfaces_xml <- xmlParse(xml_file)
    oct_surfaces_xml <- read_xml(xml_file)

    # Pull out the general information
    executable_name <- xml_find_first(oct_surfaces_xml,
                                      ".//executable//name") %>%
        xml_text()

    executable_version <- xml_find_first(oct_surfaces_xml,
                                      ".//executable//version") %>%
        xml_text()

    size_units <- xml_find_first(oct_surfaces_xml,
                                 ".//scan_characteristics//size//unit") %>%
        xml_text()
    size_x <- xml_find_first(oct_surfaces_xml,
                             ".//scan_characteristics//size//x") %>%
        xml_integer()
    size_y <- xml_find_first(oct_surfaces_xml,
                             ".//scan_characteristics//size//y") %>%
        xml_integer()
    size_z <- xml_find_first(oct_surfaces_xml,
                             ".//scan_characteristics//size//z") %>%
        xml_integer()
    voxel_size_units <- xml_find_first(oct_surfaces_xml,
                                       ".//scan_characteristics//voxel_size//unit") %>%
        xml_text()
    voxel_size_x <- xml_find_first(oct_surfaces_xml,
                                   ".//scan_characteristics//voxel_size//x") %>%
        xml_double()
    voxel_size_y <- xml_find_first(oct_surfaces_xml,
                                   ".//scan_characteristics//voxel_size//y") %>%
        xml_double()
    voxel_size_z <- xml_find_first(oct_surfaces_xml,
                                   ".//scan_characteristics//voxel_size//z") %>%
        xml_double()

    surface_num <- xml_find_first(oct_surfaces_xml,
                                  ".//surface_num") %>%
        xml_integer()


    # Pull out the surface specific information
    surface_labels <- xml_find_all(oct_surfaces_xml,
                                   ".//surface//label") %>%
        xml_text() %>%
        as.integer()

    surface_names <- xml_find_all(oct_surfaces_xml,
                                  ".//surface//name") %>%
        xml_text()
    surface_instances <- xml_find_all(oct_surfaces_xml,
                                      ".//surface//instance") %>%
        xml_text()

    # Pull out the surface y values as a single vector
    all_surfaces_y <- xml_find_all(oct_surfaces_xml,
                                   ".//surface//bscan//y") %>%
        xml_integer() + 1

    oct_data_frame <- tibble(label = rep(surface_labels, each = size_z * size_x),
                             name = rep(surface_names, each = size_z * size_x),
                             surface_id = factor(name, levels = surface_names) %>%
                                 as.numeric(),
                             # Swap the b-scan order to make it align with
                             # the Heidelberg data.
                             bscan_id = rep(rep(size_z:1, each = size_x), times = surface_num),
                             ascan_id = rep(rep(1:size_x, times = size_z), times = surface_num),
                             value = all_surfaces_y)

    # TASK: Get all the undefined ascans
    undefined_ascan_id <- xml_find_all(oct_surfaces_xml,
                                   ".//undefined_region//ascan//x") %>%
        xml_integer() + 1

    undefined_bscan_id <- xml_find_all(oct_surfaces_xml,
                                       ".//undefined_region//ascan//z") %>%
        xml_integer() + 1

    # Make sure that to reverse the b-scan IDs to match the Heidelberg order.
    reorder_bscan_ids <- tibble(bscan_id_octexplorer = 1:size_z,
                               bscan_id = size_z:1)

    undefined_region <- tibble(ascan_id = undefined_ascan_id,
                               bscan_id_octexplorer = undefined_bscan_id) %>%
        inner_join(reorder_bscan_ids)

    # Return a list of all the variables
    list(info = list(file = xml_file,
                     file_tab = oct_file_parsed_tab,
                     size_units = size_units,
                     size_x = size_x,
                     size_y = size_y,
                     size_z = size_z,
                     voxel_size_units = voxel_size_units,
                     voxel_size_x = voxel_size_x,
                     voxel_size_y = voxel_size_y,
                     voxel_size_z = voxel_size_z,
                     surface_num = surface_num),
        layers = oct_data_frame,
        undefined_region = undefined_region)
}
