# Backtesting the FTE algorithm for predicting scorelines in tipping games


# Setting up the script --------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(rlist)

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


# Download data and build bookmaker consensus model ---------------------------- 

leagues <- c("E0", "D1", "I1", "SP1")
seasons <- c("1617", "1718", "1819", "1920", "2021")

df <- get_bookmaker_data(leagues, seasons, all_var = FALSE)
df$obs_result <- transform_goals_to_result(select(df, FTHG, FTAG))

bookmakers <- list(
  B365 = c("B365H", "B365D", "B365A"), BW = c("BWH", "BWD", "BWA"),
  IW = c("IWH", "IWD", "IWA"), PS = c("PSH", "PSD", "PSA"),
  WH = c("WHH", "WHD", "WHA"), VC = c("VCH", "VCD", "VCA")
)

df[, c("prob1_bmc", "probtie_bmc", "prob2_bmc")] <- build_bmc_model(df, bookmakers, method = "power")
df[, c("xhg_dp", "xag_dp")] <- predict_result_from_pb(select(df, prob1_bmc, probtie_bmc, prob2_bmc), draws_included = F)


# Assess the model performance -------------------------------------------------

df$rps <- calculate_rps(select(df, prob1_bmc, probtie_bmc, prob2_bmc), df$obs_result)
df$mnl <- calculate_mnl(select(df, prob1_bmc, probtie_bmc, prob2_bmc), df$obs_result)
df$accuracy <- if_else(transform_prob_to_result(select(df, prob1_bmc, probtie_bmc, prob2_bmc)) == df$obs_result, 1, 0)

results_bmc = list(model_performance = list(), tipping_points = list())

results_bmc$model_performance[["overall"]] <- df %>% summarise("RPS" = mean(rps), "MNL" = mean(mnl), "Accuracy" = mean(accuracy), "Games" = length(rps))
results_bmc$model_performance[["leagues"]] <- df %>%
  group_by(Div) %>%
  summarise("RPS" = mean(rps), "MNL" = mean(mnl), "Accuracy" = mean(accuracy), "Games" = length(rps))
results_bmc$model_performance[["seasons"]] <- df %>%
  group_by(Season) %>%
  summarise("RPS" = mean(rps), "MNL" = mean(mnl), "Accuracy" = mean(accuracy), "Games" = length(rps))
results_bmc$model_performance[["leagseas"]] <- df %>%
  group_by(Season, Div) %>%
  summarise("RPS" = mean(rps), "MNL" = mean(mnl), "Accuracy" = mean(accuracy), "Games" = length(rps))


# Backtest the model for tipping games -----------------------------------------

rule <- c(4, 3, 2, 0)
alt_rule <- c(3, 2, 1, 0)

df$points <- do_strategy_backtest(
  obs_result = select(df, FTHG, FTAG),
  pred_result = select(df, xhg_dp, xag_dp),
  rule = rule, draw_tendency = F
)

results_bmc$tipping_points[["overall" ]] <- df %>% summarise("Points" = sum(points), "PPG" = sum(points) / length(points), "Games" = length(points))
results_bmc$tipping_points[["leagues" ]] <- df %>%
  group_by(Div) %>%
  summarise("Points" = sum(points), "PPG" = sum(points) / length(points), "Games" = length(points))
results_bmc$tipping_points[["seasons" ]] <- df %>%
  group_by(Season) %>%
  summarise("Points" = sum(points), "PPG" = sum(points) / length(points), "Games" = length(points))
results_bmc$tipping_points[["leagseas" ]] <- df %>%
  group_by(Season, Div) %>%
  summarise("Points" = sum(points), "PPG" = sum(points) / length(points), "Games" = length(points))


# Bivariate poisson distribution -----------------------------------------------

lambda_3 <- seq(0.01, 0.75, by = 0.01)

grid_result <- data.frame(lambda_3)

df[, c("proj_score1", "proj_score2")] <- goalmodel::expg_from_probabilities(select(df, prob1_bmc, probtie_bmc, prob2_bmc))[[1]]

for (ii in seq_along(leagues)) {
  grid_df <- filter(df, Div == leagues[ii])
  
  for (i in seq_along(lambda_3)) {
    grid_df[, c("xhg_bp", "xag_bp")] <- predict_result_from_xg(select(grid_df, proj_score1, proj_score2), draws_included = F, lambda_3[i])
    
    grid_df$points <- do_strategy_backtest(
      obs_result = select(grid_df, FTHG, FTAG),
      pred_result = select(grid_df, xhg_bp, xag_bp),
      rule = alt_rule, draw_tendency = F
    )
    
    grid_result[i, paste0("points_", leagues[ii])] <- sum(grid_df$points)
    grid_result[i, paste0("ppg_", leagues[ii])] <- sum(grid_df$points) / length(grid_df$points)
    
    message("Iteration lambda 3 = ", lambda_3[i], " in batch ", leagues[ii], " done \n")
  }
}

ggplot(grid_result, aes(lambda_3, `points_D1`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 1216, linetype = "dashed", col = "red")
ggplot(grid_result, aes(lambda_3, `points_E0`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 1718, linetype = "dashed", col = "red")
ggplot(grid_result, aes(lambda_3, `points_I1`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 1838, linetype = "dashed", col = "red")
ggplot(grid_result, aes(lambda_3, `points_SP1`)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 1699, linetype = "dashed", col = "red")


# Save output as rds -----------------------------------------------------------

list.save(results_bmc, './output/model_performances/performance_bmc.rds')

