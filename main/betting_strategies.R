# Build own football prediction model for the number of goals scored


# Setting up the script --------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(goalmodel)

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


# Download data and and do factor engineering ------------

leagues <- c("German Bundesliga")

df <- get_data_from_fte(leagues = leagues, from_season = 2020, to_season = 2020, all_var = T)

df$total_score1 <- rowMeans(select(df, xg1, nsxg1, adj_score1))
df$total_score2 <- rowMeans(select(df, xg2, nsxg2, adj_score2))
