# Function for predicting most likely result based on probabilities

# ------------------------------------------------------------------------------
# @description
# This function predicts the most likely result based on the probabilities for
# the three-way match outcome by assuming two independent poisson distributions.
# The function takes a probability matrix as an input. It should be laid out so 
# that each row is one prediction, laid out in the proper order, where each
# element is a probability and each row sum to 1. The second argument gives
# the option to exclude draws from the evaluation. The output is a
# two-column dataframe with the result depicted in terms of home and away goals.

# @param probs Three column matrix, with the probabilities for the match
#   ordered as home, draw and away.
# @param draws_included Boolean indicating if draws should be considered. The
#   default is set to TRUE.
#
# @details
# It is possible that two results are equally likely, especially when the
# expected goals are similar. In this case, the result with the lower total
# goals will be returned by the function. It is planned to also include the
# bivariate poisson distribution as an alternative method.
#
# @seealso [predict_result_from_xg()] predicts the likeliest result with the
#   expected goals matrix for the home and away team.
# ------------------------------------------------------------------------------

require(reshape2)
require(dplyr)
require(goalmodel)

predict_result_from_pb <- function(probs, draws_included = TRUE, lambda_3 = NULL) {
  results <- data.frame(matrix(NA_integer_, ncol = 0, nrow = nrow(probs)))
  
  probs <- as.matrix(probs)
  
  for (i in seq_len(nrow(probs))) {
    temp <- calculate_scorelines_from_pb(probs[i, ], lambda_3 = lambda_3)
    
    if (draws_included == F) {
      temp <- dplyr::filter(temp, home != away)
    } else {
    }
    
    maximum <- which(max(temp$value) == temp$value)
    # only the first element of maximum (i.e. the lower result) is used because
    # there can be multiple most likely results with equal likelihood
    results$home_goals[i] <- temp[maximum[1], 1]
    results$away_goals[i] <- temp[maximum[1], 2]
  }
  results
}
