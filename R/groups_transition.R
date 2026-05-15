#' Compute and map aggregated land cover transitions between two years
#'
#' Downloads MapBiomas rasters for two years, computes transition codes,
#' reclassifies into five thematic groups using `groups()`, and generates a
#' map with legend. Optionally saves the transition raster (GeoTIFF) and a CSV
#' of transition areas (hectares).
#'
#' @param aoi sf or SpatVector object.
#' @param year1 Integer start year (1985-2024).
#' @param year2 Integer end year (1985-2024, > year1).
#' @param title Optional map title.
#' @param output_dir Directory to save outputs (default working directory).
#' @param write_raster Logical; write transition raster as GeoTIFF.
#' @param write_csv Logical; write transition areas (ha) as CSV.
#' @return Invisibly list with transition SpatRaster and area data frame.
#' @export
#' @import terra
#' @import sf
#' @import ggplot2
#' @import ggspatial
#' @import dplyr
#' @importFrom stats setNames
#' @importFrom utils write.csv


groups_transition <- function(aoi, year1, year2,
                              title = NULL,
                              output_dir = NULL,
                              write_raster = FALSE,
                              write_csv = FALSE) {

  ### 1. Validation and setup

  # Capture the name of the AOI object as a string for further use
  aoi_name <- deparse(substitute(aoi))

  # Validate years
  if (!year1 %in% 1985:2024 || !year2 %in% 1985:2024)
    stop("Years must be between 1985 and 2024.")

  if (year1 >= year2)
    stop("For transition calculations year2 must be higher than year1.")

  # Validate AOI
  if (!inherits(aoi, c("sf", "SpatVector")))
    stop("AOI must be an sf or SpatVector object.")

  if (inherits(aoi, "sf"))
    aoi <- terra::vect(aoi)

  # Set output directory
  if (is.null(output_dir))
    output_dir <- getwd()

  # Validate CRS of the AOI and reproject it if needed
  url <- paste0(
    "/vsicurl/https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_",
    year1, ".tif"
  ) # Reference CRS from MapBiomas metadata
  r <- terra::rast(url)

  if (!terra::same.crs(aoi, r))
    aoi <- terra::project(aoi, terra::crs(r))


  ### 2. Helper to read cropped raster from cloud
  message("Cropping and masking AOI")

  get_data <- function(yr) {
    u <- paste0("/vsicurl/https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_",
                yr, ".tif")
    terra::crop(terra::rast(u), aoi, mask = TRUE)
  }

  aoi_mask1 <- get_data(year1)
  aoi_mask2 <- get_data(year2)


  ### 3. Combine the two masks to create transition classes
  message("Combining masked rasters")

  # This formula creates unique codes like 3014 (from class 3 to class 14)
  raw_transitions <- aoi_mask1 * 1000 + aoi_mask2

  # Aggregate the transition IDs into groups with groups function
  groups <- select(groups(), c(transition_type, transition_id))

  # Reclassify transition raster according to the groups
  transition_raster <- classify(raw_transitions, as.matrix(groups))


  ### 4. Create map
  message("Creating transition map")

  # Prepare legend (only for transitions that exist in mask)
  freq_df <- as.data.frame(freq(transition_raster))
  index_classes <- match(freq_df$value, groups$transition_id)
  legend_df <- groups()[index_classes, ]

  # Plot with ggplot (convert raster to data frame)

  raster_df <- as.data.frame(transition_raster, xy = TRUE, na.rm = TRUE)
  colnames(raster_df)[3] <- "transition_id"

  raster_df <- raster_df %>%
    left_join(legend_df, by = "transition_id") # Merge with legend information

  aoi_sf <- sf::st_as_sf(aoi) # Convert AOI boundary to sf for plotting

  if(is.null(title))
    title <- paste0("LULC Groups Transition: ", year1, " to ", year2) # default title

  # Create the map
  map <- ggplot() +
    # Raster layer
    geom_tile(data = raster_df,
              aes(x = x, y = y, fill = label)) +
    # AOI boundary overlay
    geom_sf(data = aoi_sf,
            fill = NA,
            color = "black",
            linewidth = 0.5) +
    # Color scale with mapped classes
    scale_fill_manual(
      name = "Legend",
      values = setNames(legend_df$color, legend_df$label),
      na.value = "transparent"
    ) +
    # Coordinate system
    coord_sf(crs = crs(transition_raster)) +
    # Theme and layout
    theme_minimal() +
    theme(  # X-axis text
      axis.text.x = element_text(
        size = 7,
        angle = 90,
        hjust = 0.5,
        vjust = 0.5
      ),
      # Y-axis text
      axis.text.y = element_text(
        size = 7,
        angle = 0,
        hjust = 1,
        vjust = 0.5
      ),
      # Legend parameters
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9, face = "bold"),
      legend.key.size = unit(0.5, "cm"),
      panel.grid.major = element_line(color = "gray80", linewidth = 0.2),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
      plot.margin = margin(t = 40, r = 5, b = 5, l = 5, unit = "pt")
    ) +
    # Title
    labs(title = title, x = "", y = "") +
    # Scale bar
    annotation_scale(
      location = "bl",
      width_hint = 0.125
    ) +
    # Compass (north arrow)
    annotation_north_arrow(
      location = "br",
      which_north = "true",
      height = unit(0.75, "cm"),
      width = unit(0.75, "cm"),
      pad_x = unit(0.1, "cm"),
      pad_y = unit(0.1, "cm"),
      style = north_arrow_fancy_orienteering(
        text_size = 8,
        text_col = "black"
      )
    )

  print(map) # Print the plot


  ### 5. Calculate area for transitions
  cell_area <- terra::mask(terra::cellSize(transition_raster, unit = "m"), aoi) # pixel areas in m2

  transitions_area <- terra::zonal(cell_area, transition_raster, fun = "sum")
  names(transitions_area) <- c("transition_id", "area_m2")

  transitions_area <- transitions_area %>%
    mutate(area_ha = area_m2 / 10000) %>% # convert to hectares
    right_join(legend_df, by = "transition_id") %>%
    select(-c(area_m2, transition_type)) %>%
    arrange(desc(area_ha))

  # Write raster if requested
  if (write_raster) {
    message("Writing raster")

    raster_filename <- paste0(aoi_name, "_groups_transition_raster_",
                              year1, "_", year2)
    raster_path <- file.path(output_dir, raster_filename)
    writeRaster(transition_raster, raster_path, filetype = "GTiff",
                gdal = c("COMPRESS=LZW", "TILED=YES"),
                overwrite = TRUE)
  }

  # Write CSV file if requested
  if (write_csv) {
    message("Writing CSV file")

    csv_filename <- paste0(aoi_name, "_groups_transition_",
                           year1, "_", year2, ".csv")
    csv_path <- file.path(output_dir, csv_filename)
    write.csv(transitions_area, csv_path, row.names = FALSE)
  }

  # Invisible list of objects
  invisible(list(
    transition_raster,
    transitions_area
  ))
}
