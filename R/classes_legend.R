#' MapBiomas Collection 10 legend table
#'
#' Returns a data frame with class codes, names (English and Portuguese),
#' hexadecimal colors, and hierarchical levels from the official legend.
#'
#' @keywords internal
#' @noRd
#'
#' @return A data frame with columns class_id, class_name_pt, class_name_eng,
#'         hex_color, level_hierarchy.
#' @import dplyr
#' @import tidyr
#' @importFrom stats setNames
#' @importFrom utils write.csv


classes_legend <- function() {

  # Create dataframe with all classes for map legend
  data.frame(
    class_id = c(1, 3, 4, 5, 6, 49,          # Florest and subclasses
                 10, 11, 12, 32, 29, 50,     # Herbaceous vegetation and subclasses
                 14, 15, 18, 19, 39, 20,
                 40, 62, 41, 36, 46, 47,     # Farming
                 35, 48, 9, 21,
                 22, 23, 24, 30, 75, 25,     # Non vegetated area
                 26, 33, 31,                 # Water masses
                 27, 0),                     # Not observed/available

    class_name_pt = c(
      # Floresta (categoria agregada)
      "Floresta",
      # Subclasses da Floresta
      "Formacao Florestal",
      "Formacao Savanica",
      "Mangue",
      "Floresta Alagavel",
      "Restinga Arborea",

      # Vegetacao Herbacea e Arbustiva (categoria agregada)
      "Vegetacao Herbacea e Arbustiva",
      # Subclasses da Vegetacao Herbacea
      "Campo Alagado e Area Pantanosa",
      "Formacao Campestre",
      "Apicum",
      "Afloramento Rochoso",
      "Restinga Herbacea",

      # Agropecuaria (categoria agregada)
      "Agropecuaria",
      # Subclasses da Agropecuaria
      "Pastagem",
      "Agricultura",
      "Lavoura Temporaria",
      "Soja",
      "Cana",
      "Arroz",
      "Algodao (beta)",
      "Outras Lavouras Temporarias",
      "Lavoura Perene",
      "Cafe",
      "Citrus",
      "Dende",
      "Outras Lavouras Perenes",
      "Silvicultura",
      "Mosaico de Usos",

      # Area nao Vegetada (categoria agregada)
      "Area nao Vegetada",
      # Subclasses da Area nao Vegetada
      "Praia, Duna e Areal",
      "Area Urbanizada",
      "Mineracao",
      "Usina Fotovoltaica (beta)",
      "Outras Areas nao Vegetadas",

      # Corpo Dagua (categoria agregada)
      "Corpo Dagua",
      # Subclasses do Corpo Dagua
      "Rio, Lago e Oceano",
      "Aquicultura",

      # Nao observado
      "Nao observado",
      "Nao disponivel"
    ),
    class_name_eng = c(
      # Forest
      "Forest",
      "Forest Formation",
      "Savanna Formation",
      "Mangrove",
      "Floodable Forest",
      "Wooded Sandbank Vegetation",

      # Herbaceous and Shrubby Vegetation
      "Herbaceous and Shrubby Vegetation",
      "Wetland",
      "Grassland",
      "Hypersaline Tidal Flat",
      "Rocky Outcrop",
      "Herbaceous Sandbank Vegetation",

      # Farming
      "Farming",
      "Pasture",
      "Agriculture",
      "Temporary Crop",
      "Soybean",
      "Sugar cane",
      "Rice",
      "Cotton (beta)",
      "Other Temporary Crops",
      "Perennial Crop",
      "Coffee",
      "Citrus",
      "Palm Oil",
      "Other Perennial Crops",
      "Forest Plantation",
      "Mosaic of Uses",

      # Non vegetated area
      "Non vegetated area",
      "Beach, Dune and Sand Spot",
      "Urban Area",
      "Mining",
      "Photovoltaic Power Plant (beta)",
      "Other non Vegetated Areas",

      # Water
      "Water",
      "River, Lake and Ocean",
      "Aquaculture",

      # Not Observed
      "Not Observed",
      "Not available"
    ),
    hex_color = c(
      "#1f8d49",  # Forest
      "#1f8d49",  # Forest formation
      "#7dc975",  # Savanna formation
      "#04f8fd",  # Mangrove
      "#007785",  # Floodable forest
      "#02d659",  # Wooded sandbank vegatation

      "#d6bc74",  # Herbaceous and shrubby vegetation
      "#519799",  # Wetland
      "#d6bc74",  # Grassland
      "#fc8114",  # Hypersaline tidal flat
      "#ffaa5f",  # Rocky outcrop
      "#ad5100",  # Herbaceous sandbank vegetation

      "#ffefc3",  # Farming
      "#edde8e",  # Pasture
      "#f974ed",  # Agriculture
      "#c27ba0",  # Temporary crop
      "#f5b3c8",  # Soybean
      "#db7093",  # Sugar cane
      "#c71585",  # Rice
      "#ff69b4",  # Cotton (beta)
      "#f54ca9",  # Other temporary crops
      "#d082de",  # Perennial crop
      "#d68fe2",  # Coffee
      "#9932cc",  # Citrus
      "#9065d0",  # Palm oil
      "#e6ccff",  # Other perennial crops
      "#7a5900",  # Forest plantation
      "#ffefc3",  # Mosaic of uses

      "#d4271e",  # Non vegetated area
      "#ffa07a",  # Beach, dune and sand spot
      "#d4271e",  # Urban area
      "#9c0027",  # Mining
      "#c12100",  # Photovoltaic power plant (beta)
      "#db4644",  # Other non vegetated areas

      "#2532e4",  # Water masses
      "#2532e4",  # River, lake e ocean
      "#091077",  # Aquaculture

      "#ffffff",  # Not observed
      "#ffffff"   # Not available
    ),
    stringsAsFactors = FALSE
  )
}
