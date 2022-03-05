

build_bmc_model <- function(data, bookmakers, method = "basic") {
  output <- data.frame(matrix(NA_integer_, ncol = 0, nrow = nrow(data)))

  # Adjust the probabilities to fair probabilities
  for (i in seq_along(bookmakers)) {
    temp <- implied::implied_probabilities(dplyr::select(data, bookmakers[[i]][1], bookmakers[[i]][2], bookmakers[[i]][3]), method = method)

    output[, paste0("prob1_", names(bookmakers[i]))] <- temp$probabilities[, 1]
    output[, paste0("probtie_", names(bookmakers[i]))] <- temp$probabilities[, 2]
    output[, paste0("prob2_", names(bookmakers[i]))] <- temp$probabilities[, 3]
  }

  # Derive BM consensus from fair probabilities with log probabilities
  for (i in seq_len(nrow(output))) {
    output$prob1_bmc[i] <- mean(as.matrix(dplyr::select(output[i, ], matches("prob1_"))), na.rm = T)
    output$probtie_bmc[i] <- mean(as.matrix(dplyr::select(output[i, ], matches("probtie_"))), na.rm = T)
    output$prob2_bmc[i] <- mean(as.matrix(dplyr::select(output[i, ], matches("prob2_"))), na.rm = T)
  }
  output[, c("prob1_bmc", "probtie_bmc", "prob2_bmc")]
}
