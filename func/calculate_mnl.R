# Function for Calculating the Multinomial Likelihood

# ------------------------------------------------------------------------------
# @description
# Function for calculating the multinomial likelihood to compare the goodness of
# of fit of different models.
# The function takes two arguments. The first argument (predictions) is a
# matrix with the predictions. It should be laid out so that each row is one
# prediction, laid out in the proper order, where each element is a probability
# and each row sum to 1. The second argument (observed) indicates which outcome
# was actually observed.

# @param predictions Three column matrix, with the probabilities for the
#   match ordered as home, draw and away (or in the opposite order).
#
# @param observed Numeric vector \in {1,2,3} where "1" indicates home, "2" draw,
#   and "3" away (or in the opposite order).

# @details
# Ordering in predictions matrix has to align with the observed numeric.
# E.g: if home prob is the first column in the predictions matrix, home has to
# be indicated by "1" in the observed vector (same logic if third column).
# ------------------------------------------------------------------------------

calculate_mnl <- function(predictions, observed) {
  temp <- c()

  for (i in seq_len(nrow(predictions))) {
    if (observed[i] == 1) {
      temp[i] <- predictions[i, 1]
    } else if (observed[i] == 2) {
      temp[i] <- predictions[i, 2]
    } else if (observed[i] == 3) {
      temp[i] <- predictions[i, 3]
    } else {
      temp[i] <- NA
    }
  }
  return(unlist(temp))
}
