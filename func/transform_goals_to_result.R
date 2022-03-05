# Derive three-Way result from observed goals function

# ------------------------------------------------------------------
# @description
# Function which takes the goals scored by the home and away team as an
# input and transforms it as a categorical variable with 3 levels
# (Home win, Away win, and draw)

# @param Difference Two_column matrix with observed or expected goals by the
#  home and away team ordered as home and away.
# @param output_char Optional: output_char = TRUE returns the output coded as
#   characters instead as a numeric.

# @details The coding of the output is "1" for "Home", "2" for "Draw" and "3" 
#   for "Away".
# ------------------------------------------------------------------------------

transform_goals_to_result <- function(goals, output_char = FALSE) {
  temp <- c()

  if (output_char == T) {
    for (i in seq_len(nrow(goals))) {
      if (goals[i, 1] > goals[i, 2]) {
        temp[i] <- "Home"
      } else if (goals[i, 1] < goals[i, 2]) {
        temp[i] <- "Away"
      } else if (goals[i, 1] == goals[i, 2]) {
        temp[i] <- "Draw"
      } else {
        temp[i] <- NA
      }
    }
    temp
  } else {
    for (i in seq_len(nrow(goals))) {
      if (goals[i, 1] > goals[i, 2]) {
        temp[i] <- 1
      } else if (goals[i, 1] < goals[i, 2]) {
        temp[i] <- 3
      } else if (goals[i, 1] == goals[i, 2]) {
        temp[i] <- 2
      } else {
        temp[i] <- NA
      }
    }
    temp
  }
}