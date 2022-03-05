# Backtesting the FTE algorithm for predicting scorelines in tipping games


# Setting up the script --------------------------------------------------------

rm(list = ls())

library(tidyverse)

# finds all .R and .r files within folder and sources them
source_folder <- function(folder, recursive = FALSE, ...) {
  files <- list.files(folder,
    pattern = "[.][rR]$",
    full.names = TRUE, recursive = recursive
  )
  if (!length(files)) {
    stop(simpleError(sprintf('No R files in folder "%s"', folder)))
  }
  src <- invisible(lapply(files, source, ...))
  message(sprintf('%s files sourced from folder "%s"', length(src), folder))
}
source_folder("./func")


# Download data and predict scorelines and probs for double poisson-------------

leagues <- c("German Bundesliga", "Barclays Premier League", "Italy Serie A", "Spanish Primera Division")

df <- get_data_from_fte(leagues = leagues, from_season = 2016, to_season = 2020, all_var = F)

df[, c("xhg_dp", "xag_dp")] <- predict_result_from_xg(select(df, proj_score1, proj_score2), draws_included = F)
df[, c("prob1_dp", "probtie_dp", "prob2_dp")] <- calculate_probs_from_xg(select(df, proj_score1, proj_score2))
df$obs_result <- transform_goals_to_result(select(df, score1, score2))


# Assess the model performance -------------------------------------------------

df$rps <- calculate_rps(select(df, prob1_dp, probtie_dp, prob2_dp), df$obs_result)
df$mnl <- calculate_mnl(select(df, prob1_dp, probtie_dp, prob2_dp), df$obs_result)
df$accuracy <- if_else(transform_prob_to_result(select(df, prob1_dp, probtie_dp, prob2_dp)) == df$obs_result, 1, 0)

df %>% summarise("RPS" = mean(rps), "MNL" = mean(mnl), "Accuracy" = mean(accuracy), "Games" = length(rps))
df %>%
  group_by(league) %>%
  summarise("RPS" = mean(rps), "MNL" = mean(mnl), "Accuracy" = mean(accuracy), "Games" = length(rps))
df %>%
  group_by(season) %>%
  summarise("RPS" = mean(rps), "MNL" = mean(mnl), "Accuracy" = mean(accuracy), "Games" = length(rps))
df %>%
  group_by(season, league) %>%
  summarise("RPS" = mean(rps), "MNL" = mean(mnl), "Accuracy" = mean(accuracy), "Games" = length(rps))

# The model based on FTE expected goals and the double poisson distribution to
# predict scorelines overall performs well in the last five seasons for the
# four major European Leagues based on the RPS and MNL.
# It is interesting that the method seems to perform consistently worse for the
# German Bundesliga. This might be due to less games being played every
# season in the Bundesliga and, thus, less accurate estimates of the teams'
# strength by the SPI rating of FTE.


# Backtest the model for tipping games -----------------------------------------

rule <- c(4, 3, 2, 0)
alt_rule <- c(3, 2, 1, 0)

df$points <- do_strategy_backtest(
  obs_result = select(df, score1, score2),
  pred_result = select(df, xhg_dp, xag_dp),
  rule = rule, draw_tendency = F
)

df %>% summarise("Points" = sum(points), "PPG" = sum(points) / length(points), "Games" = length(points))
df %>%
  group_by(league) %>%
  summarise("Points" = sum(points), "PPG" = sum(points) / length(points), "Games" = length(points))
df %>%
  group_by(season) %>%
  summarise("Points" = sum(points), "PPG" = sum(points) / length(points), "Games" = length(points))
df %>%
  group_by(season, league) %>%
  summarise("Points" = sum(points), "PPG" = sum(points) / length(points), "Games" = length(points))

# Similar conclusion as when assessing the overall model performance. The method
# seems to perform consistently worse for the Bundesliga on an average points
# per game (PPG) basis. Also, the method seems to perform overall best for the
# Italian Serie A. Further work could be done to compare the performance of this
# method with e.g. a bookmaker consensus model or forecasts by experts. Lastly,
# the 4,3,2,0 scoring rule seems to be better for the method than the 3,2,1,0
# rule. The first rule yields on average 35.9 % (1.437/4) of the maximum points
# per game, while the second rule yields 28.9 % (0.863/3) of the maximum points
# per game. This makes intuitively sense, since more points for the most likely
# outcome are awarded with the first rule.


# Bivariate poisson distribution -----------------------------------------------

lambda_3 <- seq(0.01, 0.75, by = 0.01)

grid_result <- data.frame(lambda_3)

for (ii in seq_along(leagues)) {
  grid_df <- filter(df, league == leagues[ii])

  for (i in seq_along(lambda_3)) {
    grid_df[, c("xhg_bp", "xag_bp")] <- predict_result_from_xg(select(grid_df, proj_score1, proj_score2), draws_included = F, lambda_3[i])

    grid_df$points <- do_strategy_backtest(
      obs_result = select(grid_df, score1, score2),
      pred_result = select(grid_df, xhg_bp, xag_bp),
      rule = alt_rule, draw_tendency = F
    )

    grid_result[i, paste0("points_", leagues[ii])] <- sum(grid_df$points)
    grid_result[i, paste0("ppg_", leagues[ii])] <- sum(grid_df$points) / length(grid_df$points)

    message("Iteration lambda 3 = ", lambda_3[i], " in batch ", leagues[ii], " done \n")
  }
}

# As an alternative to the double poisson distribution, the bivariate poisson
# is considered here. To find an appropiate covariance parameterlambda 3, a 
# grid search for the leagues of interest is conducted here. Noteably, the 
# performance of the model, i.e. the total points, can be improved by an 
# appropriate choice of lambda 3 for all leagues. 


ggplot(grid_result, aes(lambda_3, `points_German Bundesliga`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 1207, linetype = "dashed", col = "red")
ggplot(grid_result, aes(lambda_3, `points_Barclays Premier League`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 1726, linetype = "dashed", col = "red")
ggplot(grid_result, aes(lambda_3, `points_Italy Serie A`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 1827, linetype = "dashed", col = "red")
ggplot(grid_result, aes(lambda_3, `points_Spanish Primera Division`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 1720, linetype = "dashed", col = "red")


# Grid search for German Bundesliga

seasons <- c(2016, 2017, 2018, 2019, 2020)

grid_bl <- data.frame(lambda_3)

for (ii in seq_along(seasons)) {
  grid_df <- filter(df, season == seasons[ii], league == "German Bundesliga")
  
  for (i in seq_along(lambda_3)) {
    grid_df[, c("xhg_bp", "xag_bp")] <- predict_result_from_xg(select(grid_df, proj_score1, proj_score2), draws_included = F, lambda_3[i])
    
    grid_df$points <- do_strategy_backtest(
      obs_result = select(grid_df, score1, score2),
      pred_result = select(grid_df, xhg_bp, xag_bp),
      rule = rule, draw_tendency = F
    )
    
    grid_bl[i, paste0("points_", seasons[ii])] <- sum(grid_df$points)
    grid_bl[i, paste0("ppg_", seasons[ii])] <- sum(grid_df$points) / length(grid_df$points)
    
    message("Iteration lambda 3 = ", lambda_3[i], " in batch ", seasons[ii], " done \n")
  }
}

ggplot(grid_bl, aes(lambda_3, `points_2016`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 413, linetype = "dashed", col = "red")
ggplot(grid_bl, aes(lambda_3, `points_2017`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 374, linetype = "dashed", col = "red")
ggplot(grid_bl, aes(lambda_3, `points_2018`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 401, linetype = "dashed", col = "red")
ggplot(grid_bl, aes(lambda_3, `points_2019`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 392, linetype = "dashed", col = "red")
ggplot(grid_bl, aes(lambda_3, `points_2020`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 404, linetype = "dashed", col = "red")
