#' Time series of land cover composition and area differences
#'
#' Computes annual percentage and area (hectares) per land cover class over a
#' sequence of years. Produces a stacked area plot or line plot. Optionally
#' saves the results and area differences between consecutive years as CSV.
#'
#' @param aoi sf or SpatVector object.
#' @param years Numeric vector of years (default 1985:2024).
#' @param plot_type Character; either "area" (stacked area plot) or "line".
#' @param title Optional plot title.
#' @param output_dir Directory to save outputs (default working directory).
#' @param write_csv Logical; write legend, results, and area differences as CSV.
#' @return Invisibly list with legend data frame, results (year, class, percentage, area_ha),
#'         and area differences between consecutive years.
#' @export
#' @import terra
#' @import sf
#' @import ggplot2
#' @import dplyr
#' @importFrom stats setNames
#' @importFrom utils write.csv


lulc_timeseries <- function(aoi, years = 1985:2024,
                            plot_type = "area",
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


  ### 2. Helper to read cropped raster from cloud
  message("Cropping and masking AOI")

  get_data <- function(yr) {
    u <- paste0("/vsicurl/https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_",
                yr, ".tif")
    terra::crop(terra::rast(u), aoi, mask = TRUE)
  }

  ### 3. Loop over years and compute percentages and areas per class
  all_years <- list()
  for (yr in years) {
    message("Processing year ", yr)

    r <- get_data(yr)
    freq_df <- as.data.frame(freq(r))
    freq_df <- freq_df[!is.na(freq_df$value), ]
    total_pixels <- sum(freq_df$count)

    # Calculate area for transitions
    cell_area <- terra::mask(terra::cellSize(r, unit = "m"), aoi) # pixel areas in m²

    classes_area <- terra::zonal(cell_area, r, fun = "sum")
    names(classes_area) <- c("class_id", "area_m2")

    # Create a row for each class present
    yr_df <- freq_df %>%
      mutate(
        year = yr,
        percentage = count / total_pixels * 100,
        class_id = value
      ) %>%
      right_join(classes_area, by = "class_id") %>%
      mutate(area_ha = area_m2 / 10000) %>% # convert to hectares
      select(year, class_id, percentage, area_ha)

    all_years[[as.character(yr)]] <- yr_df
  }

  results <- bind_rows(all_years)


  ### 4. Calculate area differences between consecutive years

  all_classes <- sort(unique(results$class_id)) # unique classes that show up in any year

  ys <- sort(unique(results$year)) # Create vector with sorted unique years

  diff_list <- list() # Create list to store area differences

  for (i in 1:(length(ys)-1)) {
    current_year <- ys[i]
    next_year <- ys[i+1]

    # Filter data from the two years
    current_data <- results[results$year == current_year, ]
    next_data <- results[results$year == next_year, ]

    # Create vector with all classes (start with 0 for non present classes)
    current_area <- setNames(rep(0, length(all_classes)), all_classes)
    next_area <- setNames(rep(0, length(all_classes)), all_classes)

    # Fill in areas for existing classes
    for (class in current_data$class_id) {
      current_area[as.character(class)] <- current_data$area_ha[current_data$class_id == class]
    }

    for (class in next_data$class_id) {
      next_area[as.character(class)] <- next_data$area_ha[next_data$class_id == class]
    }

    # Calculate differences
    diff <- next_area - current_area

    # Data frame for differences
    df_diff <- data.frame(
      period = paste0(current_year, "-", next_year),
      matrix(diff, nrow = 1, dimnames = list(NULL, paste0("class_", all_classes))),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    diff_list[[i]] <- df_diff
  }

  # Combine all area differences
  area_diff <- do.call(rbind, diff_list)


  ### 5. Create plot
  message("Creating plot")

  # Prepare legend
  legend_full <- classes_legend()
  legend_full <- legend_full[order(legend_full$class_id), ]

  # Join with legend to get class names and colors
  results <- results %>%
    left_join(legend_full, by = "class_id") %>%
    mutate(
      class_name = ifelse(is.na(class_name_eng), paste0("Class_", class_id), class_name_eng),
      hex_color = ifelse(is.na(hex_color), "#CCCCCC", hex_color)
    )

  # Plot with ggplot
  if (plot_type == "area") {
    if (is.null(title)){
      title <- "Land Use and Land Cover Time Series" # default title
    }
    plot <- ggplot(results, aes(x = year, y = percentage, fill = class_name)) +
      geom_area(alpha = 0.8, position = "stack") +
      scale_fill_manual(
        values = setNames(results$hex_color, results$class_name),
        name = "Land use class"
      ) +
      labs(
        title = title,
        x = "Year",
        y = "Area (%)"
      ) +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 8)
      )
  } else {
    if (is.null(title)){
      title <- "Land Use and Land Cover Dynamics" # default title
    }
    # Line plot with points (for many classes it may be messy, but offered)
    plot <- ggplot(results, aes(x = year, y = percentage, color = class_name)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5) +
      scale_color_manual(
        values = setNames(results$hex_color, results$class_name),
        name = "Land use class"
      ) +
      labs(
        title = title,
        x = "Year",
        y = "Area (%)"
      ) +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 7)
      )
  }

  print(plot)

  # Write data frames to CSV files
  if (write_csv) {
    message("Writing CSV files")

    # Create a formatted string with year range
    year_range <- paste(min(years), max(years), sep = "-")

    # Classes legend data frame
    classes_legend <- classes_legend()
    csv1_filename <- "classes_legend.csv"
    csv1_path <- file.path(output_dir, csv1_filename)
    write.csv(classes_legend, csv1_path, row.names = FALSE)

    # Results data frame
    csv2_filename <- paste0(aoi_name, "_timeseries_results_",
                            year_range, ".csv")
    csv2_path <- file.path(output_dir, csv2_filename)
    write.csv(results, csv2_path, row.names = FALSE)

    # Area differences data frame
    csv3_filename <- paste0(aoi_name, "_timeseries_area_differences_",
                            year_range, ".csv")
    csv3_path <- file.path(output_dir, csv3_filename)
    write.csv(area_diff, csv3_path, row.names = FALSE)
  }

  # Invisible list of objects
  invisible(list(
    classes_legend, results, area_diff
  ))
}

