# Function for calculating three-way probabilitiesa from expected goals

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


calculate_probs_from_xg <- function(exp_goals, lambda_3 = NULL) {
  probs <- data.frame(matrix(NA_integer_, ncol = 0, nrow = nrow(exp_goals)))
  
  exp_goals <- as.matrix(exp_goals)
  
  for (i in seq_len(nrow(exp_goals))) {
    temp <- calculate_scorelines_from_xg(exp_goals[i, ], lambda_3 = lambda_3, max_goals = 10)
    
    probs$prob1[i] <- sum(temp$value[which(temp["home"] > temp["away"])])
    probs$probtie[i] <- sum(temp$value[which(temp["home"] == temp["away"])])
    probs$prob2[i] <- sum(temp$value[which(temp["home"] < temp["away"])])
  }
  probs
}
