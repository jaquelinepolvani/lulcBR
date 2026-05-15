#' Aggregate land cover transitions into five thematic classes
#'
#' Internal helper for transition mapping functions.
#'
#'@keywords internal
#' @noRd
#'
#' @return Data frame with columns: transition_id (1-5), color, label.
#'
#' @import dplyr
#' @import tidyr


groups <- function(){

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

  counter <- 10
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

  for (i in 1:length(class_list)){
    counter <- counter + 1
    transitions <- as.vector(outer(not2, class_list[[i]], "+"))
    transitions <- as.data.frame(transitions) %>%
      mutate(transition_id = counter) %>%
      rename(transition_type = transitions)

    transitions_df <- rbind(transitions_df, transitions)
  }

  for (i in 1:length(class_list1000)){
    counter <- counter + 1
    transitions <- as.vector(outer(class_list1000[[i]], not, "+"))
    transitions <- as.data.frame(transitions) %>%
      mutate(transition_id = counter) %>%
      rename(transition_type = transitions)

    transitions_df <- rbind(transitions_df, transitions)
  }

  other <- as.vector(outer(not2, not, "+"))
  other <- as.data.frame(other) %>%
    mutate(transition_id = 46) %>%
    rename(transition_type = other)

  transitions_df <- transitions_df %>%
    bind_rows(other) %>%
    mutate(
      transition_id = case_when(
        transition_id %in% c(11, 12, 16, 17, 23, 24, 28, 29, 35:46) ~ 1,
        transition_id %in% c(13, 14, 18, 19) ~ 2,
        transition_id %in% c(21, 22, 26, 27) ~ 3,
        transition_id %in% c(15, 20, 25, 30) ~ 4,
        transition_id %in% c(31, 32, 33, 34) ~ 5
      )
    )

  transitions_df <- transitions_df %>%
    mutate(
      color = case_when(
        transition_id == 1 ~ "#f3f3f3",
        transition_id == 2 ~ "#FF6B6B",
        transition_id == 3 ~ "#4CAF50",
        transition_id == 4 ~ "#3A6EA5",
        transition_id == 5 ~ "#edde8e"
      ),
      label = case_when(
        transition_id == 1 ~ "No change",
        transition_id == 2 ~ "From Forest or natural non forest areas to Pasture, agriculture or non-vegetated areas",
        transition_id == 3 ~ "From Pasture, agriculture or non-vegetated areas to Forest or natural non forest areas",
        transition_id == 4 ~ "Gain of Surface Water",
        transition_id == 5 ~ "Loss of Surface Water"
      )
    )

  return(transitions_df)
}
