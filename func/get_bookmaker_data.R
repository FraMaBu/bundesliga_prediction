# Function for downloading historic bookmaker odds

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

get_bookmaker_data <- function(leagues, seasons, all_var = TRUE) {
  list_df <- list()

  for (i in seq_along(leagues)) {
    for (ii in seq_along(seasons)) {
      temp <- tempfile()
      url <- paste0("https://www.football-data.co.uk/mmz4281/", seasons[ii], "/", leagues[i], ".csv")
      download.file(url, temp)
      data <- read.csv(temp)
      data$Season <- seasons[ii]
      list_df <- rlist::list.append(list_df, data)
    }
  }
  df <- purrr::reduce(list_df, dplyr::bind_rows)

  if (all_var == FALSE) {
    df <- dplyr::select(
      df, Div, Season , Date, HomeTeam, AwayTeam, FTHG, FTAG, B365H,
      B365D, B365A, BWH, BWD, BWA, IWH, IWD, IWA, PSH, PSD, PSA, WHH, WHD, WHA,
      VCH, VCD, VCA
    )
  } else {

  }
  df$Date <- lubridate::dmy(df$Date)
  df
}