#' Create Land Use and Land Cover (LULC) maps and statistics
#'
#' Downloads MapBiomas Collection 10 raster for a single year, crops to the AOI,
#' and generates a standardized map. Optionally saves the masked
#' raster (GeoTIFF) and a CSV of class areas (hectares).
#'
#' @param aoi sf or SpatVector object.
#' @param year Integer between 1985 and 2024.
#' @param title Optional map title.
#' @param output_dir Directory to save outputs (default working directory).
#' @param write_raster Logical; write masked raster as GeoTIFF.
#' @param write_csv Logical; write class areas (ha) as CSV.
#' @return Invisibly list with masked SpatRaster and area data frame. If no
#'         write flags are TRUE, the area data frame is returned visibly.
#' @export
#' @import terra
#' @import sf
#' @import ggplot2
#' @import ggspatial
#' @import dplyr
#' @importFrom stats setNames
#' @importFrom utils write.csv


lulc_aoi <- function(aoi, year,
                     title = NULL,
                     output_dir = NULL,
                     write_raster = FALSE,
                     write_csv = FALSE) {

  ### 1. Validation and setup

  # Capture the name of the AOI object as a string for further use
  aoi_name <- deparse(substitute(aoi))

  # Validate year
  if (!year %in% 1985:2024)
    stop("Year must be between 1985 and 2024.")

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
    year, ".tif"
  ) # Reference CRS from MapBiomas metadata
  r <- rast(url)

  if (!terra::same.crs(aoi, r))
    aoi <- terra::project(aoi, terra::crs(r))


  ### 2. Crop and mask AOI geometry
  message("Cropping and masking AOI")

  aoi_crop <- terra::crop(r, aoi)
  aoi_mask <- terra::mask(aoi_crop, aoi)


  ### 3. Plot the map
  message("Creating LULC map")

  # Prepare legend (only for classes that exist in mask)
  freq_df <- as.data.frame(freq(aoi_mask))
  legend_full <- classes_legend()
  index_classes <- match(freq_df$value, legend_full$class_id)
  legend_df <- legend_full[index_classes, ]

  # Plot with ggplot (convert raster to data frame)

  raster_df <- as.data.frame(aoi_mask, xy = TRUE, na.rm = TRUE)
  colnames(raster_df)[3] <- "class_id"

  raster_df <- raster_df %>%
    left_join(legend_df, by = "class_id") # Merge with legend information

  aoi_sf <- sf::st_as_sf(aoi) # Convert AOI boundary to sf for plotting

  if(is.null(title))
    title <- paste0("Land Use and Land Cover - ", year) # default title

  # Create the map
  map <- ggplot() +
    # Raster layer
    geom_tile(data = raster_df,
              aes(x = x, y = y, fill = class_name_eng)) +
    # AOI boundary overlay
    geom_sf(data = aoi_sf,
            fill = NA,
            color = "black",
            linewidth = 0.5) +
    # Color scale with mapped classes
    scale_fill_manual(
      name = "Legend",
      values = setNames(legend_df$hex_color, legend_df$class_name_eng),
      na.value = "transparent"
    ) +
    # Coordinate system
    coord_sf(crs = crs(aoi_mask)) +
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


  ### 4. Calculate area for classes
  cell_area <- terra::mask(terra::cellSize(aoi_mask, unit = "m"), aoi) # pixel areas in m2

  classes_area <- terra::zonal(cell_area, aoi_mask, fun = "sum")
  names(classes_area) <- c("class_id", "area_m2")

  classes_area <- classes_area %>%
    mutate(area_ha = area_m2 / 10000) %>% # convert to hectares
    right_join(legend_df, by = "class_id") %>%
    select(-area_m2) %>%
    arrange(desc(area_ha))

  # Write raster if requested
  if (write_raster) {
    message("Writing raster")

    raster_filename <- paste0(aoi_name, "_lulc_", year)
    raster_path <- file.path(output_dir, raster_filename)
    writeRaster(aoi_mask, raster_path, filetype = "GTiff",
                gdal = c("COMPRESS=LZW", "TILED=YES"),
                overwrite = TRUE)
  }

  # Write CSV file if requested
  if (write_csv) {
    message("Writing CSV file")

    # Classes area data frame
    csv_filename <- paste0(aoi_name, "_classes_area_", year, ".csv")
    csv_path <- file.path(output_dir, csv_filename)
    write.csv(classes_area, csv_path, row.names = FALSE)
  } else {
    message("Classes area in hectares")
    return(classes_area)
  }

  # Return a list with both the masked raster and the area summary
  return(invisible(list(
    aoi_mask,
    classes_area
  )))
}

