#' Filter surfaces from an XML segmentation file
#'
#' Filters surfaces from an XML segmentation to reduce the number of surfaces.
#'
#' @param xml_file An path or connection to an XML file or a character
#'   representation of XML.
#' @param keep_surfaces A vector of the surface IDs to keep in the file. Use
#'   the labels specified by OCTExplorer. By default all surfaces segmented by
#'   IRA/OCTExplorer ver. 3.8.0 are retained.
#'
#' @return The filtered XML object.
#'
#' @export
#' @importFrom xml2 read_xml xml_find_all xml_text "xml_text<-" xml_remove
filter_segmentation_xml <-
    function(
        xml_file,
        keep_surfaces = c(10, 20, 30, 40, 50, 60, 100, 110, 120, 140, 150),
        modifier = "heyexr"
        ) {

    if(any(duplicated(keep_surfaces))) {
        surfaces_duplicated <- keep_surfaces[duplicated(keep_surfaces)]

        wrn_msg <-
            paste0(
                "The following surface(s) is duplicated:\t",
                dput(surfaces_duplicated),
                "\nWe'll retain only the unique surfaces."
                )

        warning(wrn_msg)

        # Remove any duplicate surface IDs
        keep_surfaces <- unique(keep_surfaces)
    }

    keep_surfaces <- as.character(keep_surfaces)

    # Read in the XML file
    oct_surfaces_xml <- read_xml(xml_file)

    # Get the current XML surface IDs.
    current_surfaces <-
        xml_find_all(oct_surfaces_xml, "surface/label") %>%
        xml_text()

    all_keep_in_current <- all(keep_surfaces %in% current_surfaces)
    are_equal_length <- length(keep_surfaces) == length(current_surfaces)

    indices_to_remove <- which(!(current_surfaces %in% keep_surfaces))

    surfaces_to_remove <- current_surfaces[indices_to_remove]

    # If the surfaces to keep are not a strict subset of the surfaces in the
    # file...
    if(!all_keep_in_current) {
        # ... report an error:

        # Determine which surfaces to keep aren't present in the XML file.
        surfaces_not_present <-
            keep_surfaces[which(!(keep_surfaces %in% current_surfaces))]

        error_msg <-
            paste0(
                "The surfaces to keep:\t", dput(surfaces_not_present),
                "\nare not present in the list of surfaces:\t",
                dput(current_surfaces),
                "\nfrom the file:\n", xml_file,
                "\nPlease select a strict subset of the existing surfaces IDs",
                "and try again."
                )

        stop(error_msg)
    }

    # If the current surfaces are the same as the old surfaces...
    if(are_equal_length) {
        # ...then inform the user:
        msg <-
            paste(
            "The list of requested surfaces equals the list of current",
            "surfaces. Consequently, the file will be written without",
            "modification.")
        message(msg)

    } else {
        # ...otherwise retain only the surfaces to keep:

        # Find all surface nodes
        # where child "label" has a value in surfaces_to_remove.
        # To do this, construct an XPATH query of the nodes to remove:
        query_remove <-
            paste0("surface[label=", surfaces_to_remove, "]", collapse = " | ")
        # Then use the query to find the nodes:
        nodes_to_remove <- xml_find_all(oct_surfaces_xml, query_remove)

        # Remove these nodes from the XML structure.
        xml_remove(nodes_to_remove)

        # Update surface_num
        surface_num <- xml_find_all(oct_surfaces_xml, "surface_num")
        xml_text(surface_num) <- as.character(length(keep_surfaces))

        # Update modification date: mm/dd/yyy
        # Get current date and time:
        current_datetime <- Sys.time()
        current_date <- strftime(current_datetime, "%m/%d/%Y")
        current_time <- strftime(current_datetime, "%H:%M:%S")

        mod_date <- xml_find_all(oct_surfaces_xml, "modification/date")
        xml_text(mod_date) <- current_date

        # Update modification time: hh:mm:ss
        mod_time <- xml_find_all(oct_surfaces_xml, "modification/time")
        xml_text(mod_time) <- current_time

        # Update modifier
        mod_modifier <- xml_find_all(oct_surfaces_xml, "modification/modifier")
        xml_text(mod_modifier) <- modifier
    }

    # Write the file.
    return(oct_surfaces_xml)
}
