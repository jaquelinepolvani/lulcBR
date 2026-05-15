#' Linear trend analysis for aggregated land cover groups
#'
#' Computes annual area (hectares) for Forest, Herbaceous, Farming,
#' Non vegetated Areas, and Water classes over a time series. Fits a linear regression
#' per group and returns slope, p-value, and R2. Produces a faceted plot.
#'
#' @param aoi sf or SpatVector object.
#' @param years Numeric vector of years (default 1985:2024).
#' @param title Optional plot title.
#' @param output_dir Directory to save outputs (default working directory).
#' @param write_csv Logical; write trend statistics as CSV.
#' @return Invisibly list with data (long format) and trends (slope, p_value, r_squared).
#' @export
#' @import terra
#' @import sf
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' @importFrom stats setNames
#' @importFrom utils write.csv

trend_analysis <- function(aoi, years = 1985:2024,
                           title = NULL,
                           output_dir = NULL,
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


  ### 2. Define aggregated groups
  groups_map <- list(
    Forest          = c(3, 4, 5, 6, 49),
    Herbaceous      = c(11, 12, 32, 29, 50),
    Farming         = c(15, 39, 20, 40, 62, 41, 46, 47, 35, 48, 9, 21),
    `Non vegetated` = c(23, 24, 30, 75, 25),
    Water           = c(33, 31)
  )


  ### 3. Pre-calculate Pixel Area Map (The Precise Way)
  message("Computing pixel area weights")

  # Calculate cell area
  r_crop <- terra::crop(r, aoi)
  cell_area <- terra::mask(terra::cellSize(r_crop, unit = "ha"), aoi) # pixel areas in ha

  # Mask AOI once to use as a template for all years
  aoi_mask <- terra::mask(cell_area, aoi)


  ### 4. Extract annual area using weighted frequency
  res_list <- list()
  for (yr in years) {
    message("Processing year ", yr)
    url <- paste0("/vsicurl/https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_",
                  yr, ".tif")

    r <- terra::crop(terra::rast(url), aoi, mask = TRUE)

    # CROSS-TABULATION: This matches land cover codes with their specific pixel areas
    zonal_area <- terra::zonal(aoi_mask, r, fun = "sum", na.rm = TRUE)
    names(zonal_area) <- c("value", "area_ha")

    row <- data.frame(year = yr)
    for (g in names(groups_map)) {
      codes <- groups_map[[g]]
      # Sum the areas of all pixels belonging to the group
      total_group_area <- sum(zonal_area$area_ha[zonal_area$value %in% codes], na.rm = TRUE)
      row[[g]] <- total_group_area
    }
    res_list[[as.character(yr)]] <- row
  }

  df_long <- bind_rows(res_list) %>%
    pivot_longer(cols = -year, names_to = "group", values_to = "area_ha")


  ### 5. Linear trend statistics
  trends <- df_long %>%
    group_by(group) %>%
    group_modify(~ {
      mod <- lm(area_ha ~ year, data = .x)
      sum_mod <- summary(mod)
      data.frame(
        slope = coef(mod)[2],
        p_value = sum_mod$coefficients[2, 4],
        r_squared = sum_mod$r.squared
      )
    })


  ### 6. Visualization

  if(is.null(title))
    title <- paste0("Land Cover Trends: ", years[1], " to ", years[length(years)]) # default title

  stats_caption <- paste0(
    "Linear trend statistics:\n",
    paste(sprintf("%s: slope = %.2f ha/yr, R2 = %.3f",
                  trends$group, trends$slope, trends$r_squared),
          collapse = "\n")
  )

  group_colors <- c("Forest"="#1B5E20", "Herbaceous"="#66BB6A", "Farming"="#F4D03F", "Non vegetated"="#90A4AE", "Water"="#1E88E5")

  p <- ggplot(df_long, aes(x = year, y = area_ha, colour = group)) +
    geom_line(aes(group = group), colour = "black", linewidth = 0.3, alpha = 0.7) +
    geom_point(size = 0.8, alpha = 0.6) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, fill = "grey80", alpha = 0.3, linewidth = 0.9) +
    scale_colour_manual(values = group_colors, guide = "none")

  facet_wrap(~ group, scales = "free_y", ncol = 3)

  p <- p + labs(title = title, x = "Year", y = expression(Area ~ (ha)), caption = stats_caption) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.caption = element_text(size = 8, hjust = 0, face = "italic"),
      strip.background = element_rect(fill = "grey90", colour = NA),
      strip.text = element_text(face = "bold", size = 10),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      axis.title = element_text(face = "bold")
    )

  print(p)

  message("Trend analysis completed.")

  # Write CSV file if requested

  if (write_csv) {
    message("Writing CSV file")

    year_range <- paste(min(years), max(years), sep = "-") # string with year range
    csv_filename <- paste0(aoi_name, "_land_cover_trends_",
                           year_range, ".csv")
    csv_path <- file.path(output_dir, csv_filename)
    write.csv(trends, csv_path, row.names = FALSE)
  }

  # Invisible list of objects
  invisible(list(
    data = df_long,
    trends = trends
  )
  )
}
