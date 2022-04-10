dss_singles <- function(rating_1, rating_2, winner, q = 1.824, k = 0.275) {
  
  # Calculates the results of a singles match in tennis according to the 
  #   updated Dynamic Rating System (DSS) as used by the Royal Dutch Lawn 
  #   Tennis Association (KNLTB) since the year 2022.
  #
  # Input:
  #   rating_1: Rating of player 1           (numeric, 4 decimals).
  #   rating_2: Rating of player 2           (numeric, 4 decimals).
  #   winner:   Number of the winning player (numeric, value {1, 1.5, 2}).
  #   q         Logistic growth rate         (numeric, default value 1.824).
  #   k         Maximum change in rating     (numeric, default value 0.275).
  #
  # Output:
  #   List with match results for all players:
  #     old_rating_1: Old rating of player 1 (from input).
  #     old_rating_2: Old rating of player 2 (from input).
  #     prob_1:       Probability to win for player 1.
  #     prob_2:       Probability to win for player 2.
  #     result_1:     Result for player 1.
  #     result_2:     Result for player 2.
  #     new_rating_1: New rating of player 1.
  #     new_rating_2: New rating of player 2.
  
  prob_1 <- 1 / (1 + exp(-q * (rating_2 - rating_1)))
  prob_2 <- 1 / (1 + exp(-q * (rating_1 - rating_2)))
  
  result_1 <- k * (prob_1 - abs(2 - winner))
  result_2 <- k * (prob_2 - abs(1 - winner))
  
  new_rating_1 <- rating_1 + result_1
  new_rating_2 <- rating_2 + result_2
  
  return(list(
    "old_rating_1" = round(rating_1, 4),
    "old_rating_2" = round(rating_2, 4),
    "prob_1"       = round(prob_1, 2),
    "prob_2"       = round(prob_2, 2),
    "result_1"     = round(result_1, 4),
    "result_2"     = round(result_2, 4),
    "new_rating_1" = round(new_rating_1, 4),
    "new_rating_2" = round(new_rating_2, 4)
  ))
}

# # Example
# dss_singles(
#   rating_1 = 4.0614,
#   rating_2 = 4.1387,
#   winner   = 2
# )