#' Animated GIF of land cover time series
#'
#' Generates an animated map (GIF) showing land cover change over a sequence of
#' years. Uses cloud‑optimized reads and optional aggregation for performance.
#'
#' @param aoi sf or SpatVector object.
#' @param years Numeric vector of years (default 1985:2024).
#' @param title Optional main title.
#' @param output_file Path to save the GIF. If NULL, user is prompted.
#' @param fps Frames per second (default 3).
#' @param width,height GIF dimensions in pixels (default 700×550).
#' @param res_factor Aggregation factor (1 = 30 m, 2 = 60 m, 4 = 120 m). Default 2.
#' @param north_arrow_location Position: "tr", "br", "tl", "bl" (default "br").
#' @param scale_location Position of scale bar: "bl", "br", "tl", "tr" (default "bl").
#' @param preview Logical; show preview in RStudio viewer (default TRUE).
#' @return Invisibly returns the animated plot object.
#' @export
#' @import terra
#' @import sf
#' @import ggplot2
#' @import ggspatial
#' @import dplyr
#' @import gganimate
#' @import gifski


lulc_animation <- function(aoi, years = 1985:2024,
                           title = NULL,
                           output_file = NULL,
                           fps = 3,
                           width = 700,
                           height = 550,
                           res_factor = 2,
                           north_arrow_location = "br",
                           scale_location = "bl",
                           preview = TRUE) {

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


  ### 3. Loop over years to get data frames from rasters

  # Load MapBiomas legend first
  legend_full <- classes_legend()
  legend_full <- legend_full[order(legend_full$class_id), ]

  color_pal <- legend_full$hex_color
  names(color_pal) <- as.character(legend_full$class_id)

  class_labels <- legend_full$class_name_eng
  names(class_labels) <- as.character(legend_full$class_id)

  # Loop
  all_years <- list()
  for (yr in years) {
    message("Processing year ", yr)
    r <- get_data(yr)

    # Optional aggregation to speed up rendering (reduces number of pixels)
    if (res_factor > 1) {
      r <- aggregate(r, fact = res_factor, fun = "modal", na.rm = TRUE)
    }

    df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)

    names(df)[3] <- "class"
    df$class <- factor(df$class, levels = as.numeric(names(color_pal)))
    df$year <- yr

    all_years[[as.character(yr)]] <- df
  }

  results <- bind_rows(all_years)


  ### 4. Create map

  if (is.null(title))
    title <- "Land Use and Land Cover Dynamics" # default title

  # Plot with ggplot

  map <- ggplot(results, aes(x = x, y = y, fill = class)) +
    geom_tile() +
    scale_fill_manual(
      values = color_pal,
      na.value = "#CCCCCC",
      name = "Legend",
      labels = class_labels
    ) +

    # Coordinate system
    coord_sf(crs = crs(r)) +

    # Scale bar in metric units (automatically shows km or m)
    annotation_scale(
      location = scale_location,
      width_hint = 0.25,
      style = "bar",
      text_face = "bold",
      text_cex = 0.8,
      line_width = 0.8,
      pad_x = unit(0.3, "cm"),
      pad_y = unit(0.3, "cm")
    ) +

    # North arrow (user‑selected position, padded)
    annotation_north_arrow(
      location = north_arrow_location,
      which_north = "true",
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm"),
      style = north_arrow_fancy_orienteering(text_size = 10)
    ) +

    labs(
      title = title,
      subtitle = "Year: {frame_time}"
    ) +

    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12),
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(face = "bold"),
      plot.margin = margin(5, 5, 5, 5)
    ) +

    transition_time(year) +
    ease_aes("linear")

  ### 5. Render animation
  message("Rendering animation. This may take a while depending on data size.")

  animation <- gganimate::animate(
    map,
    nframes = length(years),
    fps = fps,
    width = width,
    height = height,
    renderer = gifski_renderer(loop = TRUE)
  )

  ### 6. Preview and save
  if (preview) {
    message("Previewing animation in RStudio Viewer.")
    print(animation)
  }

  if (is.null(output_file) && interactive()) {
    answer <- readline("Do you want to save the animation as a GIF? (y/n): ")
    if (tolower(answer) == "y") {
      default_name <- "lulc_animation.gif"
      cat("Enter the file name (will be saved in current working directory)\n")
      name <- readline(paste0("[default: ", default_name, "]: "))
      if (name == "")
        name <- default_name
      if (!grepl("\\.gif$", name, ignore.case = TRUE))
        name <- paste0(name, ".gif")
      output_file <- file.path(getwd(), name)
    }
  }

  if (!is.null(output_file)) {
    anim_save(output_file, animation)
    message("GIF saved to: ", normalizePath(output_file))
  }

  return(invisible(animation))
}
