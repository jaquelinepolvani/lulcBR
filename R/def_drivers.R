#' Deforestation driver classification for forest transitions
#'
#' Internal helper that maps transition codes to specific
#' deforestation drivers (pasture, soybean, sugarcane, urbanization,
#' mining, etc.) with associated colors and labels.
#'
#' @keywords internal
#' @noRd
#'
#' @return Data frame with columns: transition_type (raw code), transition_id
#'         (driver ID 1-14), color (hex), label (driver description).
#' @import dplyr
#' @import tidyr

def_drivers <- function(){

  forest <- c(3, 4, 5, 6, 49)
  herb <- c(11, 12, 32, 29, 50)
  farm <- c(15, 39, 20, 40, 62, 41, 46, 47, 35, 48, 9, 21)
  nonveg <- c(23, 24, 30, 75, 25)
  water <- c(33, 31)
  not <- c(27, 0)

  fo <- forest * 1000
  he <- herb * 1000
  fa <- farm * 1000
  non <- nonveg * 1000
  wa <- water * 1000
  not2 <- not * 1000

  remained <- as.vector(outer(fo, forest, "+"))
  remained <- as.data.frame(remained) %>%
    mutate(transition_id = 1) %>%
    rename(transition_type = remained)

  pasture <- as.vector(outer(fo, farm[1], "+"))
  pasture <- as.data.frame(pasture) %>%
    mutate(transition_id = 2) %>%
    rename(transition_type = pasture)

  soybean <- as.vector(outer(fo, farm[2], "+"))
  soybean <- as.data.frame(soybean) %>%
    mutate(transition_id = 3) %>%
    rename(transition_type = soybean)

  sugar_cane <- as.vector(outer(fo, farm[3], "+"))
  sugar_cane <- as.data.frame(sugar_cane) %>%
    mutate(transition_id = 4) %>%
    rename(transition_type = sugar_cane)

  temp_crops <- as.vector(outer(fo, c(farm[4], farm[5], farm[6]), "+"))
  temp_crops <- as.data.frame(temp_crops) %>%
    mutate(transition_id = 5) %>%
    rename(transition_type = temp_crops)

  coffee <- as.vector(outer(fo, farm[7], "+"))
  coffee <- as.data.frame(coffee) %>%
    mutate(transition_id = 6) %>%
    rename(transition_type = coffee)

  citrus <- as.vector(outer(fo, farm[8], "+"))
  citrus <- as.data.frame(citrus) %>%
    mutate(transition_id = 7) %>%
    rename(transition_type = citrus)

  palm_oil <- as.vector(outer(fo, farm[9], "+"))
  palm_oil <- as.data.frame(palm_oil) %>%
    mutate(transition_id = 8) %>%
    rename(transition_type = palm_oil)

  prn_crops <- as.vector(outer(fo, farm[10], "+"))
  prn_crops <- as.data.frame(prn_crops) %>%
    mutate(transition_id = 9) %>%
    rename(transition_type = prn_crops)

  plantation <- as.vector(outer(fo, farm[11], "+"))
  plantation <- as.data.frame(plantation) %>%
    mutate(transition_id = 10) %>%
    rename(transition_type = plantation)

  urban <- as.vector(outer(fo, nonveg[2], "+"))
  urban <- as.data.frame(urban) %>%
    mutate(transition_id = 11) %>%
    rename(transition_type = urban)

  mining <- as.vector(outer(fo, nonveg[3], "+"))
  mining <- as.data.frame(mining) %>%
    mutate(transition_id = 12) %>%
    rename(transition_type = mining)

  other <- as.vector(outer(fo, c(herb, farm[12], nonveg[1], nonveg[4], nonveg[5],
                                 water, not), "+"))
  other <- as.data.frame(other) %>%
    mutate(transition_id = 13) %>%
    rename(transition_type = other)

  non_app <- as.vector(outer(c(he, fa, non, wa, not2), c(forest, herb, farm,
                                                         nonveg, water, not), "+"))
  non_app <- as.data.frame(non_app) %>%
    mutate(transition_id = 14) %>%
    rename(transition_type = non_app)

  transitions_df <- rbind(remained, pasture, soybean, sugar_cane, temp_crops,
                          coffee, citrus, palm_oil, prn_crops, plantation,
                          urban, mining, other, non_app)

  transitions_df <- transitions_df %>%
    mutate(
      color = case_when(
        transition_id == 1 ~ "darkgreen",
        transition_id == 2 ~ "#edde8e",
        transition_id == 3 ~ "#f5b3c8",
        transition_id == 4 ~ "#db7093",
        transition_id == 5 ~ "#f54ca9",
        transition_id == 6 ~ "#d68fe2",
        transition_id == 7 ~ "#9932cc",
        transition_id == 8 ~ "#9065d0",
        transition_id == 9 ~ "#e6ccff",
        transition_id == 10 ~ "#7a5900",
        transition_id == 11 ~ "#d4271e",
        transition_id == 12 ~ "#9c0027",
        transition_id == 13 ~ "#091077",
        transition_id == 14 ~ "#f3f3f3"
      ),
      label = case_when(
        transition_id == 1 ~ "Remained forest",
        transition_id == 2 ~ "Pasture",
        transition_id == 3 ~ "Soybean",
        transition_id == 4 ~ "Sugar cane",
        transition_id == 5 ~ "Other temporary crops",
        transition_id == 6 ~ "Coffee",
        transition_id == 7 ~ "Citrus",
        transition_id == 8 ~ "Palm oil",
        transition_id == 9 ~ "Other perennial crops",
        transition_id == 10 ~ "Plantation",
        transition_id == 11 ~ "Urbanization",
        transition_id == 12 ~ "Mining",
        transition_id == 13 ~ "Other drivers",
        transition_id == 14 ~ "Non applicable"
      )
    )

  return(transitions_df)
}
