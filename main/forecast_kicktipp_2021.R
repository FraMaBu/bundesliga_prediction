# Forecasting of scorelines for Kicktipp 2021


# Setting up the script --------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(openxlsx)

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


# Setting up parameters --------------------------------------------------------

league <- c("German Bundesliga")
season <- 2021
rule <- c(4, 3, 2, 0)
draw_rule <- FALSE


# Download data and predict scorelines -----------------------------------------

df <- get_data_from_fte(leagues = league, from_season = season, to_season = season, all_var = F)
df$matchweek <- rep(1:34, each=9) 

df[, c("xhg_1", "xag_1")] <- predict_result_from_xg(select(df, proj_score1, proj_score2), draws_included = F, lambda_3 = 0.15)

outcome <- df %>% filter(is.na(score1) == F)
outcome$obs_result <- transform_goals_to_result(select(outcome, score1, score2))

outcome <- outcome %>% select(date, matchweek, league, team1, team2, xhg_1, xag_1, score1, score2, obs_result)

# Allocate points for past matches ---------------------------------------------

outcome$points <- do_strategy_backtest(
  obs_result = select(outcome, score1, score2),
  pred_result = select(outcome, xhg_1, xag_1),
  rule = rule, draw_tendency = draw_rule
)

# Safe outputs to xlsx -----------------------------------------------------------

list_of_datasets <- list("forecasts" = df, "outcome" = outcome)
write.xlsx(list_of_datasets, file =  paste("./output/kicktipp_2021//Prediction_KT", Sys.Date(), ".xlsx", sep = "_"))
