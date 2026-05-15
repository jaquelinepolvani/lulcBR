#' Forest loss, gain, and stability classification
#'
#' Internal helper that maps transition codes to four
#' categories: remained forest, forest loss, forest gain, and non-applicable.
#' Used by forest transition mapping
#'
#' @keywords internal
#' @noRd
#'
#' @return Data frame with columns: transition_type (raw code), transition_id
#'         (1-4), color (hex), label (category description).
#' @import dplyr
#' @import tidyr


forest_related <- function() {

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

  loss <- as.vector(outer(fo, c(herb, farm, nonveg, water, not), "+"))
  loss <- as.data.frame(loss) %>%
    mutate(transition_id = 2) %>%
    rename(transition_type = loss)

  gain <- as.vector(outer(c(he, fa, non, wa, not2), forest, "+"))
  gain <- as.data.frame(gain) %>%
    mutate(transition_id = 3) %>%
    rename(transition_type = gain)

  non_app <- as.vector(outer(c(he, fa, non, wa, not2), c(herb, farm, nonveg, water, not), "+"))
  non_app <- as.data.frame(non_app) %>%
    mutate(transition_id = 4) %>%
    rename(transition_type = non_app)

  transitions_df <- rbind(remained, loss, gain, non_app)

  transitions_df <- transitions_df %>%
    mutate(
      color = case_when(
        # No changed forest
        transition_id == 1 ~ "darkgreen",
        # Forest loss (forest to non-forest)
        transition_id == 2 ~ "#FF6B6B",
        # Forest gain (non-forest to forest)
        transition_id == 3 ~ "lightgreen",
        # Default
        TRUE ~ "#f3f3f3"
      ),
      label = case_when(
        # No changed forest
        transition_id == 1 ~ "Remained forest",
        # Forest loss (forest to non-forest)
        transition_id == 2 ~ "Forest loss",
        # Forest gain (non-forest to forest)
        transition_id == 3 ~ "Forest gain",
        # Default
        TRUE ~ "Non applicable"
      )
    )

  return(transitions_df)
}
