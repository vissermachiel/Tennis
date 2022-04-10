dss_doubles <- function(rating_1_1, rating_1_2, rating_2_1, rating_2_2, winner,
                        theta = 0.5, q = 2.012, k = 0.275) {
  
  # Calculates the results of a doubles match in tennis according to the 
  #   Dynamic Rating System (DSS) as used by the Royal Dutch Lawn Tennis 
  #   Association (KNLTB) since the year 2022.
  #
  # Input:
  #   rating_1_1: Rating of player 1 of team 1   (numeric, 4 decimals).
  #   rating_1_2: Rating of player 2 of team 1   (numeric, 4 decimals).
  #   rating_2_1: Rating of player 1 of team 2   (numeric, 4 decimals).
  #   rating_2_2: Rating of player 2 of team 2   (numeric, 4 decimals).
  #   winner:     Team number of winning team    (numeric, value {1, 1.5, 2}).
  #   theta:      Weight factor for team average (numeric, default value 0.5).
  #   q           Logistic growth rate           (numeric, default value 2.012).
  #   k           Maximum change in rating       (numeric, default value 0.275).
  #
  # Output:
  #   List with match results for all players:
  #     old_rating_1_1: Old rating of player 1 of team 1 (from input).
  #     old_rating_1_2: Old rating of player 2 of team 1 (from input).
  #     old_rating_2_1: Old rating of player 1 of team 2 (from input).
  #     old_rating_2_2: Old rating of player 2 of team 2 (from input).
  #     avg_1:          Average rating of team 1.
  #     avg_2:          Average rating of team 2.
  #     prob_1:         Probability to win for team 1.
  #     prob_2:         Probability to win for team 2.
  #     result_1:       Result for team 1.
  #     result_2:       Result for team 2.
  #     new_rating_1_1: New rating of player 1 of team 1.
  #     new_rating_1_2: New rating of player 2 of team 1.
  #     new_rating_2_1: New rating of player 1 of team 2.
  #     new_rating_2_2: New rating of player 2 of team 2.
  
  avg_1 <- theta * rating_1_1 + (1 - theta) * rating_1_2
  avg_2 <- theta * rating_2_1 + (1 - theta) * rating_2_2
  
  prob_1 <- 1 / (1 + exp(-q * (avg_2 - avg_1)))
  prob_2 <- 1 / (1 + exp(-q * (avg_1 - avg_2)))
  
  result_1 <- k * (prob_1 - abs(2 - winner))
  result_2 <- k * (prob_2 - abs(1 - winner))
  
  new_rating_1_1 <- rating_1_1 + result_1
  new_rating_1_2 <- rating_1_2 + result_1
  new_rating_2_1 <- rating_2_1 + result_2
  new_rating_2_2 <- rating_2_2 + result_2
  
  return(list(
    "old_rating_1_1" = round(rating_1_1, 4),
    "old_rating_1_2" = round(rating_1_2, 4),
    "old_rating_2_1" = round(rating_2_1, 4),
    "old_rating_2_2" = round(rating_2_2, 4),
    "avg_1"          = round(avg_1, 4), 
    "avg_2"          = round(avg_2, 4),
    "prob_1"         = round(prob_1, 2),
    "prob_2"         = round(prob_2, 2),
    "result_1"       = round(result_1, 4),
    "result_2"       = round(result_2, 4),
    "new_rating_1_1" = round(new_rating_1_1, 4),
    "new_rating_1_2" = round(new_rating_1_2, 4),
    "new_rating_2_1" = round(new_rating_2_1, 4),
    "new_rating_2_2" = round(new_rating_2_2, 4)
  ))
}

# # Example
# dss_doubles(
#   rating_1_1 = 3.9492,
#   rating_1_2 = 3.5908,
#   rating_2_1 = 4.7905,
#   rating_2_2 = 3.1285,
#   winner     = 2
# )