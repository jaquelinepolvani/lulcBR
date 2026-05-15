#' Transition matrix between two years for aggregated land cover classes
#'
#' Computes the area (hectares) that changed from each original aggregated class
#' (forest, herbaceous, farming, non-vegetated, water) to each target class
#' between two years. Returns a 5×5 matrix with rows = original class,
#' columns = target class. Optionally saves the matrix as CSV.
#'
#' @param aoi sf or SpatVector object.
#' @param year1 Integer start year (1985-2024).
#' @param year2 Integer end year (1985-2024, > year1).
#' @param output_dir Directory to save CSV (default working directory).
#' @return Returns a data frame (5×5 matrix) with areas in hectares.
#' @export
#' @import terra
#' @import sf
#' @import dplyr
#' @importFrom stats setNames
#' @importFrom utils write.csv


transition_matrix <- function(aoi, year1, year2,
                              output_dir = NULL) {

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

  matrix_helper <- function(){

    class_list <- list(
      forest <- c(3, 4, 5, 6, 49),
      herb <- c(11, 12, 32, 29, 50),
      farm <- c(15, 39, 20, 40, 62, 41, 46, 47, 35, 48, 9, 21),
      nonveg <- c(23, 24, 30, 75, 25),
      water <- c(33, 31))

    not <- c(27, 0)

    class_list1000 <- list(
      fo <- forest * 1000,
      he <- herb * 1000,
      fa <- farm * 1000,
      non <- nonveg * 1000,
      wa <- water * 1000)

    not2 <- not * 1000

    counter <- 0
    transitions_df <- data.frame()

    for (i in 1:length(class_list)){
      counter <- counter + 1
      transitions <- as.vector(outer(fo, class_list[[i]], "+"))
      transitions <- as.data.frame(transitions) %>%
        mutate(transition_id = counter) %>%
        rename(transition_type = transitions)

      transitions_df <- rbind(transitions_df, transitions)
    }

    for (i in 1:length(class_list)){
      counter <- counter + 1
      transitions <- as.vector(outer(he, class_list[[i]], "+"))
      transitions <- as.data.frame(transitions) %>%
        mutate(transition_id = counter) %>%
        rename(transition_type = transitions)

      transitions_df <- rbind(transitions_df, transitions)
    }

    for (i in 1:length(class_list)){
      counter <- counter + 1
      transitions <- as.vector(outer(fa, class_list[[i]], "+"))
      transitions <- as.data.frame(transitions) %>%
        mutate(transition_id = counter) %>%
        rename(transition_type = transitions)

      transitions_df <- rbind(transitions_df, transitions)
    }

    for (i in 1:length(class_list)){
      counter <- counter + 1
      transitions <- as.vector(outer(non, class_list[[i]], "+"))
      transitions <- as.data.frame(transitions) %>%
        mutate(transition_id = counter) %>%
        rename(transition_type = transitions)

      transitions_df <- rbind(transitions_df, transitions)
    }

    for (i in 1:length(class_list)){
      counter <- counter + 1
      transitions <- as.vector(outer(wa, class_list[[i]], "+"))
      transitions <- as.data.frame(transitions) %>%
        mutate(transition_id = counter) %>%
        rename(transition_type = transitions)

      transitions_df <- rbind(transitions_df, transitions)
    }
    return(transitions_df)
  }

  # Aggregate the transition IDs into groups with matrix_helper function
  groups <- matrix_helper()

  # Reclassify transition raster according to the groups
  transition_raster <- classify(raw_transitions, as.matrix(groups))

  # Prepare legend (only for transitions that exist in mask)
  freq_df <- as.data.frame(freq(transition_raster))
  index_classes <- match(freq_df$value, groups$transition_id)
  legend_df <- matrix_helper()[index_classes, ]


  ### 4. Calculate area for transitions
  cell_area <- terra::mask(terra::cellSize(transition_raster, unit = "m"), aoi) # pixel areas in m²

  transitions_area <- terra::zonal(cell_area, transition_raster, fun = "sum")
  names(transitions_area) <- c("transition_id", "area_m2")

  transitions_area <- transitions_area %>%
    mutate(area_ha = area_m2 / 10000) %>% # convert to hectares
    right_join(legend_df, by = "transition_id") %>%
    select(-c(area_m2, transition_type)) %>%
    arrange(desc(area_ha))

  # Create transition matrix
  tr_mx <- matrix(NA, nrow = 5, ncol = 5)
  rownames(tr_mx) <- c("forest", "herbaceous", "farming", "non vegetated area", "water")
  colnames(tr_mx) <- c("forest", "herbaceous", "farming", "non vegetated area", "water")
  transitions_area <- transitions_area %>% arrange(transition_id)
  for (i in 1:nrow(transitions_area)){
    tr <- transitions_area$transition_id[i]
    row <- ceiling(tr/5)
    column <- ((tr - 1) %% 5) + 1
    tr_mx[row, column] <- transitions_area$area_ha[i]
  }

  tr_mx <- as.data.frame(tr_mx)

  # Write CSV file
  message("Writing CSV file")
  csv_filename <- paste0(aoi_name, "_transition_matrix_", year1, "_", year2, ".csv")
  csv_path <- file.path(output_dir, csv_filename)
  write.csv(tr_mx, csv_path, row.names = TRUE)
}

