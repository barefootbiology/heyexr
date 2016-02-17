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
#' @importFrom XML xmlParse xmlRoot xmlValue xmlTreeParse xmlToDataFrame
#' @importFrom magrittr %>%
read_segmentation_xml <- function(xml_file) {
    oct_file_parsed_tab = gsub(xml_file, pattern="\\.xml$",
                               replacement=".txt", perl = TRUE)

    # Loading the XML version
    oct_surfaces_xml = xmlParse(xml_file)

    # TASK: Update the code to parse through the file sequentially as that may
    #       save time.

    # Parse the information about scan unit, scan dimensions,
    # voxel units, and voxel dimensions
    size_units <- xmlRoot(oct_surfaces_xml)[["scan_characteristics"]][["size"]][["unit"]] %>%
        xmlValue
    size_x <- xmlRoot(oct_surfaces_xml)[["scan_characteristics"]][["size"]][["x"]] %>%
        xmlValue %>%
        as.numeric
    size_y <- xmlRoot(oct_surfaces_xml)[["scan_characteristics"]][["size"]][["y"]] %>%
        xmlValue %>%
        as.numeric
    size_z <- xmlRoot(oct_surfaces_xml)[["scan_characteristics"]][["size"]][["z"]] %>%
        xmlValue %>%
        as.numeric

    # This data will be used later for converting voxels to µm for reports.
    voxel_size_units <- xmlRoot(oct_surfaces_xml)[["scan_characteristics"]][["voxel_size"]][["unit"]] %>%
        xmlValue
    voxel_size_x <- xmlRoot(oct_surfaces_xml)[["scan_characteristics"]][["voxel_size"]][["x"]] %>%
        xmlValue %>%
        as.numeric
    voxel_size_y <- xmlRoot(oct_surfaces_xml)[["scan_characteristics"]][["voxel_size"]][["y"]] %>%
        xmlValue %>%
        as.numeric
    voxel_size_z <- xmlRoot(oct_surfaces_xml)[["scan_characteristics"]][["voxel_size"]][["z"]] %>%
        xmlValue %>%
        as.numeric

    surface_num <- xmlRoot(oct_surfaces_xml)[["surface_num"]]

    # Parse the XML file if that has not been performed previously.
    # Otherwise, load a tab-delimited version of the parsed XML.
    oct_data_frame <- NULL
    if(!file.exists(oct_file_parsed_tab)) {

        # CONVERT THE FOLLOWING CODE TO SOME KIND OF APPLY statement or better!
        # Elements 8-18 in the XML file correspond to the 11 surfaces detected
        # by Iowa Reference Algorithms.

        oct_list <- list()
        # TASK: Update this code to work with any number of surface results
        surface_index <- xmlTreeParse(xml_file)$doc$children$surfaces %>%
            names %>%
            grepl(pattern = "^surface$", perl = TRUE) %>%
            which
        for(i in surface_index) {
            surface_label <- xmlRoot(oct_surfaces_xml)[[i]][[1]] %>%
                xmlValue() %>%
                as.numeric
            surface_name <- xmlRoot(oct_surfaces_xml)[[i]][[2]] %>%
                xmlValue()

            # There are 61 "bscan" subelements.
            # Combine these as a long format table, using 1-61 as the covariate (bscan_id)
            # CODE HERE

            surface_temp_list <- list()
            for (j in 1:size_y+2) {
                surface_temp_list[[j]] = xmlRoot(oct_surfaces_xml)[[i]][[j]] %>%
                    xmlToDataFrame %>%
                    tbl_df %>%
                    mutate(label=surface_label, name=surface_name, bscan_id=j-2)
            }
            oct_list[[i]] <- bind_rows(surface_temp_list)
        }

        # Combine all the surfaces into one file
        oct_data_frame <- bind_rows(oct_list) %>%
            rename(value=text) %>%
            mutate(value=as.numeric(value),
                   bscan_id=as.numeric(bscan_id),
                   label=as.numeric(label)) %>%
            mutate(label_2=as.numeric(factor(as.character(label)))) %>%
            group_by(label) %>%
            mutate(ascan_id = rep(1:size_x, size_y)) %>%
            ungroup

        # Get the depth rank for each layer
        layer_z_order <- oct_data_frame %>%
            select(name, label) %>%
            distinct %>%
            mutate(layer_z_order = rank(as.numeric(label))) %>%
            select(-label)

        # Add that value to the data
        oct_data_frame <- oct_data_frame %>%
            inner_join(layer_z_order)

        # EXPORT
        # Save to file for quicker loading
        oct_data_frame %>%
            write.table(oct_file_parsed_tab,
                        sep="\t", quote=FALSE, col.names=TRUE, row.names=FALSE)

    } else {
        oct_data_frame <- read.delim(oct_file_parsed_tab, sep="\t", header=TRUE) %>%
            tbl_df
    }

    # Return a list of all the variables
    list(info = list(file = xml_file,
                     file_tab = oct_file_parsed_tab,
                     size_units=size_units,
                     size_x=size_x,
                     size_y=size_y,
                     size_z=size_z,
                     voxel_size_units=voxel_size_units,
                     voxel_size_x=voxel_size_x,
                     voxel_size_y=voxel_size_y,
                     voxel_size_z=voxel_size_z),
         layers = oct_data_frame) %>%
        return()
}
