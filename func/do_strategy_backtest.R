# Function to backtest strategies in result tipping games

# ------------------------------------------------------------------------------
# @description
# This function backtests the historic performance of strategies/algorithm to
# predict the final result of matches. The function takes four arguments as
# inputs. The first argument is a matrix containing the observed results in the 
# backtesting period. The second argument is a matrix containing the predicted 
# results for these matches in the historic time horizon. The third argument is 
# vector with the points rule for the tipping game, where points are given
# for the correct result, goal difference, and tendency. The forth argument
# controls how draws are handled, i.e. if predicting a draw but not the "right"
# draw yields the points for goal difference or tendency. The output is a
# vector with the points for each match.

# @param obs_result Two-column matrix, with the results of the matches
#   ordered as home goals and away goals and each row representing a result.
# @param pred_result Two-column matrix, with the predicted match results
#   ordered as home goals and away goals and each row representing a result.
# @param rule Vector of size with the points rule ordered as points for
#   correct result, goal difference, tendency, and wrong result.
# @param draw_tendency Boolean controlling if draws, which are not the correct
#   result, receive points for the goal difference or tendency. Default is set
#   to TRUE.
#
# @details For an example of a tipping game with such rules refer to kicktipp.de
# ------------------------------------------------------------------------------
do_strategy_backtest <- function(obs_result, pred_result, rule, draw_tendency = TRUE) {
  points <- c()

  for (i in seq_len(nrow(obs_result))) {
    # match needs to be converted to three-way outcome
    obs_out <- transform_goals_to_result(obs_result[i, ])
    pred_out <- transform_goals_to_result(pred_result[i, ])
    # goal difference of match is calculated
    obs_gd <- obs_result[i, 1] - obs_result[i, 2]
    pred_gd <- pred_result[i, 1] - pred_result[i, 2]

    if (obs_result[i, 1] == pred_result[i, 1] &
      obs_result[i, 2] == pred_result[i, 2]) {
      points[i] <- rule[1]
    } else if (ifelse(draw_tendency == TRUE,
      # draws do not get points for correct goal difference (default)
      obs_gd == pred_gd & obs_result[i, 1] - obs_result[i, 2] != 0,
      # draws do get points for correct goal difference
      obs_gd == pred_gd
    )) {
      points[i] <- rule[2]
    } else if (obs_out == pred_out) {
      points[i] <- rule[3]
    } else {
      points[i] <- rule[4]
    }
  }
  points
}