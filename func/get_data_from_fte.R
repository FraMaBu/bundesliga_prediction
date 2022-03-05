# Function for downloading data from Five Thirty Eight

#-------------------------------------------------------------------------------
# @description
# This function downloads the data provided by the Five Thirty Eight API, which
# can be used to model the outcome of football matches. The function takes four
# arguments. The first argument is a vector indicating for which leagues the
# data should be downloaded. The second and third argument define the time
# period for which match results should be retrieved (in seasons). The forth
# argument is an optional filtering of the intermediate variables, which are
# used by the Five Thirty Eight algorithm to calculate the expected goals and,
# thus, also the three way probabilities. The output is a dataframe with the
# data provided by FTE per match.
#
# @param leagues Character vector with the names of the leagues.
# @param from_season Numeric indicating the starting season (including) for the
#   data.
# @param to_season Numeric indicating the ending season (including) for the
#   data.
# @param all_var Optional Boolean. If FALSE intermediate variables are
#   excluded from the output. Default is set to TRUE.
# ------------------------------------------------------------------------------

get_data_from_fte <- function(leagues, from_season, to_season, all_var = TRUE) {
  temp <- tempfile()
  download.file("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv", temp)
  data <- read.csv(temp)
  data$date <- lubridate::ymd(data$date)

  data <- data[data$league %in% leagues, ]
  data <- data[data$season >= from_season & data$season <= to_season, ]

  # eliminate intermediate variables used to derive prob and exp goals
  if (all_var == FALSE) {
    data <- dplyr::select(
      data, season, date, league, team1, team2, prob1, probtie,
      prob2, proj_score1, proj_score2, score1, score2
    )
  } else {

  }
  data
}