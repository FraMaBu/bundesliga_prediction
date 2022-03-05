#Function for predicting most likely result based on expected goals

# ------------------------------------------------------------------------------
# @description
# This function predicts the most likely result based on the expected goals for
# the home and away team by assuming two independent poisson distributions or 
# a bivariate poisson distribution.
# The function takes an expected goal matrix as an input and gives the option
# to exclude draws from the evaluation. The output is a two-column dataframe
# with the result depicted in terms of home and away goals.

# @param exp_goals Two-column matrix with the expected goals for the match
#   ordered as (expected) home goals and (expected) away goals.
# @param draws_included Boolean indicating if draws should be considered. The
#   default is set to TRUE.
# @param likelihood Optional Boolean indicating if the likelihood should for the
#   likeliest result should be also returned. Default is set to FALSE.
#
# @details
# It is possible that two results are equally likely, especially when the
# expected goals are similar. In this case, the result with the lower total
# goals will be returned by the function. It is planned to also include the
# bivariate poisson distribution as an alternative method.
#
# @seealso [predict_result_from_pb()] predicts the likeliest result with the
#   probability matrix for the three-way match outcome.
# ------------------------------------------------------------------------------

require(reshape2)
require(dplyr)

predict_result_from_xg <- function(exp_goals, draws_included = TRUE, lambda_3 = NULL, likelihood = FALSE) {
  results <- data.frame(matrix(NA_integer_, ncol = 0, nrow = nrow(exp_goals)))

  exp_goals <- as.matrix(exp_goals)

  for (i in seq_len(nrow(exp_goals))) {
   temp <- calculate_scorelines_from_xg(exp_goals[i, ], lambda_3 = lambda_3)
    
    if (draws_included == F) {
      temp <- dplyr::filter(temp, home != away)
    } else {
    }

    maximum <- which(max(temp$value) == temp$value)
    # only the first element of maximum (i.e. the lower result) is used because
    # there can be multiple most likely results with equal likelihood
    
    if (likelihood == TRUE) {
      results$home_goals[i] <- temp[maximum[1], 1]
      results$away_goals[i] <- temp[maximum[1], 2]
      results$likelihood[i] <- temp[maximum[1], 3]
      
    } else {
    results$home_goals[i] <- temp[maximum[1], 1]
    results$away_goals[i] <- temp[maximum[1], 2]
    }
  }
  results
}
