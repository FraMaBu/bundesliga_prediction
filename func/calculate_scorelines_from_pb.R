#Function for calculating score lines from three-way probabilites

# ------------------------------------------------------------------------------
# @description
# This function predicts score lines based on the three-way probabilities for
# the home and away team by assuming two independent poisson distributions or a
# a bivariate poisson distribution.
# The function takes three arguments and input. The first input is a three-way
# probability matrix. The second argument controls if the bivariate poisson
# distribution should be used to model the score lines. The third argument
# indicates the range of goals for which the likelihood of scorelines should be
# evaluated the output is a two column dataframe with the likelihood for all
# scorelines.

# @param probs Three-column matrix with the probabilites for the match outcome
#   ordered as home, draw, and away.
# @param lambda_3 Optional positive integer indicating the covariance between
#   home and away goals for the bivariate poisson. Default is set to NULL 
#   assuming a double poisson distribution. 
# @param max_goals Optional integer indicating the range of goals for which all 
#   possible combinations of scorelines should be evaluated. Default is set to
#   (from 0 to) 6 goals.
#
# @details
# The function is used by internally the functions [predict_result_from_xg] and 
# [calculate_probs_from_xg] to derive the score lines from expected goals.

# @seealso [calculate_scorelines_from_xg()] predicts the likelihood for scorelines
#   with the probability matrix for the three-way match outcome.
#   
# ------------------------------------------------------------------------------

require(reshape2)
require(dplyr)
require(goalmodel)

calculate_scorelines_from_pb <- function(probs, lambda_3 = NULL, max_goals = 6) {
  exp_goals <- goalmodel::expg_from_probabilities(probs)[[1]]
  
  c <- matrix(NA_integer_, ncol = max_goals + 1, nrow = max_goals + 1)
  colnames(c) <- 0:max_goals 
  rownames(c) <- 0:max_goals 
  
  temp <- reshape2::melt(c)
  colnames(temp) <- c("home", "away", "value")
  
  if (is.null(lambda_3) == TRUE) {
    temp$value <- dpois(temp$home, exp_goals[1]) * dpois(temp$away, exp_goals[2])
  }
  else {
    temp$value <- extraDistr::dbvpois(temp$home, temp$away, exp_goals[1], exp_goals[2], lambda_3)
  }
  temp
}
