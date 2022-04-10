dss_singles_old <- function(rating_1, rating_2, winst_1) {
  
  # Berekent de wedstrijdresultaten voor het enkelspel volgens het Dynamisch 
  #   Speelsterkte Systeem (DSS), zoals gehanteerd door de KNLTB.
  #
  # Input:
  #   rating_1: Actuele speelsterkte van speler 1 (numeric, 4 decimalen).
  #   rating_2: Actuele speelsterkte van speler 2 (numeric, 4 decimalen).
  #   winst_1:  Boolean om aan te geven of speler 1 heeft gewonnen 
  #             van speler 2 {TRUE, FALSE}.
  #
  # Output:
  #   List met wedstrijdresultaten voor beide spelers.
  
  verschil_12 <- abs(rating_1 - rating_2)
  
  # Is het verschil in actuele rating (op het moment dat de uitslag van de 
  # partij verwerkt wordt) tussen de tegenstanders groter dan 1?
  if (verschil_12 > 1) {
    
    # Wint de speler met de beste rating?
    if ((winst_1 == TRUE & rating_1 < rating_2) |
        (winst_1 == FALSE & rating_1 > rating_2)) {
      
      # Het resultaat van deze partij telt niet mee voor het DSS.
      resultaat_1 <- NA
      resultaat_2 <- NA
      
    } else {
      # Voor de winnende speler is het resultaat 1 minder dan de actuele rating 
      # (op het moment van verwerking) van de tegenstander. 
      # Voor de verliezende speler is het resultaat 1 meer dan de actuele rating 
      # (op het moment van verwerking) van de tegenstander.
      if (winst_1 == TRUE) {
        resultaat_1 <- rating_2 - 1
        resultaat_2 <- rating_1 + 1
        
      } else {
        resultaat_1 <- rating_2 + 1
        resultaat_2 <- rating_1 - 1
      }
    }
    
  } else {
    # Voor de winnende speler is het resultaat 1 minder dan de actuele rating 
    # (op het moment van verwerking) van de tegenstander. 
    # Voor de verliezende speler is het resultaat 1 meer dan de actuele rating 
    # (op het moment van verwerking) van de tegenstander.
    if (winst_1 == TRUE) {
      resultaat_1 <- rating_2 - 1
      resultaat_2 <- rating_1 + 1
      
    } else {
      resultaat_1 <- rating_2 + 1
      resultaat_2 <- rating_1 - 1
    }
  }
  
  if (winst_1 != TRUE & winst_1 != FALSE) {
    resultaat_1 <- NA
    resultaat_2 <- NA
  }
  
  return(list("resultaat_1" = resultaat_1, 
              "resultaat_2" = resultaat_2))
}

# dss_singles_old(
#   rating_1 = 5.1666,
#   rating_2 = 4.6301,
#   winst_1 = TRUE
# )
