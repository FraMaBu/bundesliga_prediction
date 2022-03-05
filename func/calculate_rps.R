# Function for Calculating the Rank Probability Score

# ------------------------------------------------------------------------------
# @description
# Function for calculating the rank probability score to compare the goodness of
# of fit of different models.
# The function takes two arguments. The first argument (predictions) is a
# matrix with the predictions. It should be laid out so that each row is one
# prediction, laid out in the proper order, where each element is a probability
# and each row sum to 1. The second argument (observed) indicates which outcome
# was actually observed.

# @param predictions Three column matrix, with the probabilities for the
#   match ordered as home, draw and away (or in the opposite order).
# @param observed Numeric vector \in {1,2,3} where "1" indicates home, "2" draw,
#   and "3" away (or in the opposite order).

# @details
# Ordering in predictions matrix has to align with the observed numeric.
# E.g: if home prob is the first column in the predictions matrix, home has to
# be indicated by "1" in the observed vector (same logic if third column).
# ------------------------------------------------------------------------------

calculate_rps <- function(predictions, observed) {
  ncat <- ncol(predictions)
  npred <- nrow(predictions)

  rps <- numeric(npred)

  for (rr in 1:npred) {
    obsvec <- rep(0, ncat)
    obsvec[observed[rr]] <- 1
    cumulative <- 0
    for (i in 1:ncat) {
      cumulative <- cumulative + (sum(predictions[rr, 1:i]) - sum(obsvec[1:i]))^2
    }
    rps[rr] <- (1 / (ncat - 1)) * cumulative
  }
  return(rps)
}