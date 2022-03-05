# Three-way Result From Three-way Probabilities Function

# ------------------------------------------------------------------------------
# @description
# Function to transform the three-way probabilities into the expected match
# outcome. The function takes one matrix as an argument. It should be laid out
# so that each row is one prediction, laid out in the proper order, where
# each element is a probability and each row sum to 1.

# @param prob Three column matrix, with the probabilities for the
#   match ordered as home, draw and away.
# @param output_char Optional: output_char = TRUE returns the output coded as
#   characters instead as a numeric.

# The coding of the output is "1" for "Home", "2" for "Draw" and "3" for "Away".
# ------------------------------------------------------------------------------

transform_prob_to_result <- function(prob, output_char = F) {
  temp <- c()

  if (output_char == T) {
    for (i in seq_len(nrow(prob))) {
      if (max(prob[i, ]) == prob[i, 1]) {
        temp[i] <- "Home"
      } else if (max(prob[i, ]) == prob[i, 2]) {
        temp[i] <- "Draw"
      } else if (max(prob[i, ]) == prob[i, 3]) {
        temp[i] <- "Away"
      } else {
        temp[i] <- NA
      }
    }
    return(temp)
  } else {
    for (i in seq_len(nrow(prob))) {
      if (max(prob[i, ]) == prob[i, 1]) {
        temp[i] <- 1
      } else if (max(prob[i, ]) == prob[i, 2]) {
        temp[i] <- 2
      } else if (max(prob[i, ]) == prob[i, 3]) {
        temp[i] <- 3
      } else {
        temp[i] <- NA
      }
    }
    return(temp)
  }
}