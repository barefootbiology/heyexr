#' Read layer segmentation output from Iowa Reference Algorithms.
#'
#' Reads the layer segmentation as output by the Iowa Reference Algorithms (XML format).
#'
#' @param xml_file path to the segmentation XML file.
#'
#' @return A list containing the header information, layer segmentation, and undefined
#'     regions.
#'
#' @export
#' @importFrom dplyr tbl_df filter rename mutate mutate group_by ungroup select distinct inner_join bind_rows
#' @importFrom magrittr %>%
#' @importFrom xml2 read_xml xml_find_first xml_find_all xml_text xml_integer xml_double as_list
#' @importFrom purrr map
#' @importFrom tibble as_tibble tibble
#' @importFrom readr type_convert col_date col_integer col_double col_time
read_segmentation_xml <- function(xml_file) {

    # Loading the XML version
    oct_surfaces_xml <- read_xml(xml_file)

    header_elements <-
        c(
            "version",
            "executable",
            "modification",
            "scan_characteristics",
            "unit",
            "surface_size",
            "surface_num"
        )

    header_nested <-
        header_elements %>%
        set_names(header_elements) %>%
        map(
            ~xml_find_first(oct_surfaces_xml, paste0(".//", .x)) %>%
                as_list()
        )

    update_cols <-
        cols("executable_date" = col_date(format = "%m/%d/%Y"),
             "modification_date" = col_date(format = "%m/%d/%Y"),
             "modification_time" = col_time(format = "%H:%M:%S"),
             "scan_characteristics_size_x" = col_integer(),
             "scan_characteristics_size_y" = col_integer(),
             "scan_characteristics_size_z" = col_integer(),
             "scan_characteristics_voxel_size_x" = col_double(),
             "scan_characteristics_voxel_size_y" = col_double(),
             "scan_characteristics_voxel_size_z" = col_double(),
             "surface_size_x" = col_integer(),
             "surface_size_z" = col_integer(),
             "surface_num" = col_integer()
        )

    header <-
        header_nested %>%
        delist_singletons() %>%
        list_to_onerow() %>%
        map(as_vector) %>%
        as_tibble() %>%
        type_convert(update_cols)

    header$xml_file <- xml_file

    # Variables needed for further computations below.
    size_x <- header$scan_characteristics_size_x
    size_z <- header$scan_characteristics_size_z
    surface_num <- header$surface_num

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

    oct_data_frame <-
        tibble(
            label = rep(surface_labels, each = size_z * size_x),
            name = rep(surface_names, each = size_z * size_x),
            surface_id =
                factor(name, levels = surface_names) %>%
                as.numeric(),
                # Swap the b-scan order to make it align with
                # the Heidelberg data.
            bscan_id = rep(rep(size_z:1, each = size_x), times = surface_num),
            ascan_id = rep(rep(1:size_x, times = size_z), times = surface_num),
            value = all_surfaces_y
            )

    # Get all the undefined ascans
    undefined_ascan_id <- xml_find_all(oct_surfaces_xml,
                                   ".//undefined_region//ascan//x") %>%
        xml_integer() + 1

    undefined_bscan_id <- xml_find_all(oct_surfaces_xml,
                                       ".//undefined_region//ascan//z") %>%
        xml_integer() + 1


    # Make sure that to reverse the B-scan IDs to match the Heidelberg order.
    reorder_bscan_ids <- tibble(bscan_id_octexplorer = 1:size_z,
                               bscan_id = size_z:1)

    undefined_region <- tibble(ascan_id = undefined_ascan_id,
                               bscan_id_octexplorer = undefined_bscan_id) %>%
        inner_join(reorder_bscan_ids)

    info <-
        list(
            file = xml_file,
            size_units = header$scan_characteristics_size_unit,
            size_x = header$scan_characteristics_size_x,
            size_y = header$scan_characteristics_size_y,
            size_z = header$scan_characteristics_size_z,
            voxel_size_units = header$scan_characteristics_voxel_size_unit,
            voxel_size_x = header$scan_characteristics_voxel_size_x,
            voxel_size_y = header$scan_characteristics_voxel_size_y,
            voxel_size_z = header$scan_characteristics_voxel_size_z,
            surface_num = header$surface_num
            )

    # Return a list of all the variables
    list(
        info = info, # Retained for compatibility
        header = header,
        layers = oct_data_frame,
        undefined_region = undefined_region
        )
}
