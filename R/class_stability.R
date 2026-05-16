#' Pixel‑wise land cover change frequency map and statistics
#'
#' Computes the number of times each pixel changed between consecutive years
#' over the specified time series. Produces a map of change counts and returns
#' a frequency table with pixel counts, area (hectares), and percentage per
#' change level.
#'
#' @param aoi sf or SpatVector object.
#' @param years Numeric vector of years (default 1985:2024). Must be sorted.
#' @param title Optional map title.
#' @param output_dir Directory to save outputs (default working directory).
#' @param write_raster Logical; write change raster as GeoTIFF.
#' @param write_csv Logical; write frequency table as CSV.
#' @return Invisibly list with change_raster (SpatRaster) and frequency_table
#'         (data frame).
#' @export
#'
#' @examples
#' \dontrun{
#' class_stability(aoi, years = 2000:2020)
#' }
#'
#' @importFrom terra rast vect crop mask project same.crs crs cellSize zonal freq classify values writeRaster aggregate patches expanse global distance ifel match
#' @import sf
#' @import ggplot2
#' @import ggspatial
#' @import dplyr
#' @import grDevices
#' @importFrom stats setNames
#' @importFrom utils write.csv

class_stability <- function(aoi, years = 1985:2024,
                            title = NULL,
                            output_dir = NULL,
                            write_raster = FALSE,
                            write_csv = FALSE) {

  ### 1. Validation and setup

  # Capture the name of the AOI object as a string for further use
  aoi_name <- deparse(substitute(aoi))

  # Validate years
  if (any(years < 1985) | any(years > 2024))
    stop("All years must be between 1985 and 2024 (inclusive).")

  if (years[1] >= years[2])
    stop("For timeseries calculations final year must be higher than first one.")

  if (!is.numeric(years) || any(is.na(years)))
    stop("Both years must be numeric and non-missing.")

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
    years[1], ".tif"
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


  ### 3. Loop over years

  # Calculate cell area
  r_crop <- terra::crop(r, aoi)
  cell_area <- terra::mask(terra::cellSize(r_crop, unit = "m"), aoi) # pixel areas in m²

  # Create zero raster
  change_raster <- terra::rast(r_crop)
  terra::values(change_raster) <- 0

  # Loop
  prev_raster <- get_data(years[1])
  for (i in 2:length(years)) {
    message("Processing transition: ", years[i-1], " -> ", years[i])
    curr_raster <- get_data(years[i])
    changed <- curr_raster != prev_raster
    changed[is.na(changed)] <- 0
    change_raster <- change_raster + changed
    prev_raster <- curr_raster
  }
  change_raster <- terra::mask(change_raster, aoi)


  ### 4. Frequency table with area (m²)
  freq_table <- terra::zonal(cell_area, change_raster, fun = "sum", na.rm = TRUE)
  names(freq_table) <- c("n_changes", "area_m2")

  counts <- as.data.frame(terra::freq(change_raster))
  counts <- counts[!is.na(counts$value), ]

  freq_table <- freq_table %>%
    mutate(
      count_pixels = counts$count[match(n_changes, counts$value)],
      area_ha = area_m2 / 10000, # convert to hectares
      percentage = (area_ha / sum(area_ha)) * 100
    ) %>%
    select(n_changes, count_pixels, area_ha, percentage)


  ### 5. Output and plot using ggplot2

  # Convert raster to data frame for ggplot2
  change_df <- as.data.frame(change_raster, xy = TRUE, na.rm = FALSE)
  colnames(change_df)[3] <- "n_changes"
  change_df <- change_df[!is.na(change_df$n_changes), ]

  aoi_sf <- sf::st_as_sf(aoi) # Convert AOI boundary to sf for plotting

  # Define color palette
  max_c <- max(change_df$n_changes, na.rm = TRUE)
  if (max_c == 0) {
    colors <- c("#f3f3f3")
  } else {
    colors <- c("#f3f3f3", grDevices::colorRampPalette(c("#FFF7BC", "#D95F0E", "#990000"))(max_c))
  }

  if (is.null(title)) {
    title <- paste0("LULC Changes (", years[1], "-", years[length(years)], ")")
  }

  # Create the map
  map <- ggplot() +
    # Raster layer
    geom_tile(data = change_df,
              aes(x = x, y = y, fill = factor(n_changes))) +
    # AOI boundary overlay
    geom_sf(data = aoi_sf,
            fill = NA,
            color = "black",
            linewidth = 0.5) +
    # Color scale with mapped classes
    scale_fill_manual(
      name = "Number of changes",
      values = colors,
      na.value = "transparent"
    ) +
    # Coordinate system
    coord_sf(crs = crs(change_raster)) +
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

  message("Stability analysis completed.")

  # Write raster if requested
  year_range <- paste(min(years), max(years), sep = "-") # string with year range

  if (write_raster) {
    message("Writing raster")
    # Change raster
    raster_filename <- paste0(aoi_name, "_stability_", year_range)
    raster_path <- file.path(output_dir, raster_filename)
    writeRaster(change_raster, raster_path, filetype = "GTiff",
                gdal = c("COMPRESS=LZW"),
                overwrite = TRUE)
  }

  # Write CSV file if requested
  if (write_csv) {
    message("Writing CSV file")
    # Frequency table
    csv_filename <- paste0(aoi_name, "_stability_", year_range, ".csv")
    csv_path <- file.path(output_dir, csv_filename)
    write.csv(freq_table, csv_path, row.names = FALSE)
  }

  # Invisible list of objects
  invisible(list(
    change_raster = change_raster,
    frequency_table = freq_table))
}
