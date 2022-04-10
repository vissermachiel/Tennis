DSS_dubbel <- function(rating_1, rating_2, rating_3, rating_4, winst_12) {
  
  # Berekent de wedstrijdresultaten voor het dubbelspel volgens het Dynamisch 
  #   Speelsterkte Systeem (DSS), zoals gehanteerd door de KNLTB.
  #
  # Input:
  #   rating_1: Actuele speelsterkte van speler 1 (numeric, 4 decimalen).
  #   rating_2: Actuele speelsterkte van speler 2 (numeric, 4 decimalen).
  #   rating_3: Actuele speelsterkte van speler 3 (numeric, 4 decimalen).
  #   rating_4: Actuele speelsterkte van speler 4 (numeric, 4 decimalen).
  #   winst_12: Boolean om aan te geven of spelers 1 en 2 hebben gewonnen 
  #             van spelers 3 en 4 {TRUE, FALSE}.
  #
  # Output:
  #   List met wedstrijdresultaten voor alle vier de spelers.
  
  gemiddeld_12 <- round((rating_1 + rating_2) / 2, 4)
  gemiddeld_34 <- round((rating_3 + rating_4) / 2, 4)
  verschil_12  <- abs(rating_1 - rating_2)
  verschil_34  <- abs(rating_3 - rating_4)
  som_12       <- rating_1 + rating_2
  som_34       <- rating_3 + rating_4
  
  # Is in de partij een combinatie betrokken waarin het 
  # verschil in rating tussen de partners groter is dan 2.5?
  if (verschil_12 > 2.5 | verschil_34 > 2.5) {
    
    # De partij levert voor geen van de spelers een DSS resultaat op.
    resultaat_1 <- NA
    resultaat_2 <- NA
    resultaat_3 <- NA
    resultaat_4 <- NA
    
  } else {
    # Is in de partij minimaal één combinatie betrokken waarin het verschil in 
    # rating tussen de partners tussen de 1.5 en 2.5 ligt of hier gelijk aan is?
    if ((verschil_12 >= 1.5 & verschil_12 <= 2.5) | 
        (verschil_34 >= 1.5 & verschil_34 <= 2.5)) {
      
      # Is het verschil tussen de beide combinaties opgeteld 1.5 of kleiner?
      if (abs(som_12 - som_34) <= 1.5) {
        
        # Bij winst is het behaalde resultaat 0.5 minder dan het cijfer 
        # van de eigen rating.
        # Bij verlies is het behaalde resultaat 0.5 meer dan het cijfer 
        # van de eigen rating.
        if (winst_12 == TRUE) {
          resultaat_1 <- rating_1 - 0.5
          resultaat_2 <- rating_2 - 0.5
          resultaat_3 <- rating_3 + 0.5
          resultaat_4 <- rating_4 + 0.5
        }
        
        if (winst_12 == FALSE) {
          resultaat_1 <- rating_1 + 0.5
          resultaat_2 <- rating_2 + 0.5
          resultaat_3 <- rating_3 - 0.5
          resultaat_4 <- rating_4 - 0.5
        }
        
      } else {
        # Als de opgeteld zwakste combinatie wint, dan is 
        # voor de winnaars het behaalde resultaat 1 minder dan het cijfer 
        # van de eigen rating. 
        # Voor de verliezers is het behaalde resultaat 1 meer dan het cijfer 
        # van de eigen rating.
        if (winst_12 == TRUE & som_12 > som_34) {
          resultaat_1 <- rating_1 - 1
          resultaat_2 <- rating_2 - 1
          resultaat_3 <- rating_3 + 1
          resultaat_4 <- rating_4 + 1
        }
        
        if (winst_12 == FALSE & som_12 < som_34) {
          resultaat_1 <- rating_1 + 1
          resultaat_2 <- rating_2 + 1
          resultaat_3 <- rating_3 - 1
          resultaat_4 <- rating_4 - 1
        }
        
        # Als de opgeteld sterkste combinatie wint levert de partij voor 
        # geen van de spelers een DSS resultaat op.
        if (winst_12 == TRUE & som_12 < som_34) {
          resultaat_1 <- NA
          resultaat_2 <- NA
          resultaat_3 <- NA
          resultaat_4 <- NA
        }
        
        if (winst_12 == FALSE & som_12 > som_34) {
          resultaat_1 <- NA
          resultaat_2 <- NA
          resultaat_3 <- NA
          resultaat_4 <- NA
        }
      }
      
    } else {
      # Is het verschil tussen de beide combinaties opgeteld 1.5 of kleiner?
      if (abs(som_12 - som_34) <= 1.5) {
        
        # Is het verschil tussen de partners van beide combinaties 
        # kleiner dan 1?
        if (verschil_12 < 1 & verschil_34 < 1) {
          
          # Bij winst is het behaalde resultaat 1 minder dan het 
          # gemiddelde cijfer van de ratings van de tegenstanders.
          # Bij verlies is het behaalde resultaat 1 meer dan het 
          # gemiddelde cijfer van de ratings van de tegenstanders.
          if (winst_12 == TRUE) {
            resultaat_1 <- gemiddeld_34 - 1
            resultaat_2 <- gemiddeld_34 - 1
            resultaat_3 <- gemiddeld_12 + 1
            resultaat_4 <- gemiddeld_12 + 1
          }
          
          if (winst_12 == FALSE) {
            resultaat_1 <- gemiddeld_34 + 1
            resultaat_2 <- gemiddeld_34 + 1
            resultaat_3 <- gemiddeld_12 - 1
            resultaat_4 <- gemiddeld_12 - 1
          }
          
        } else {
          # Bij winst is het behaalde resultaat 0.5 minder dan het cijfer 
          # van de eigen rating.
          # Bij verlies is het behaalde resultaat 0.5 meer dan het cijfer 
          # van de eigen rating.
          if (winst_12 == TRUE) {
            resultaat_1 <- rating_1 - 0.5
            resultaat_2 <- rating_2 - 0.5
            resultaat_3 <- rating_3 + 0.5
            resultaat_4 <- rating_4 + 0.5
          }
          
          if (winst_12 == FALSE) {
            resultaat_1 <- rating_1 + 0.5
            resultaat_2 <- rating_2 + 0.5
            resultaat_3 <- rating_3 - 0.5
            resultaat_4 <- rating_4 - 0.5
          }
        }
        
      } else {
        # Als de opgeteld zwakste combinatie wint, dan is 
        # voor de winnaars het behaalde resultaat 1 minder dan het 
        # gemiddelde cijfer van de ratings van de tegenstanders. 
        # Voor de verliezers is het behaalde resultaat 1 meer dan het 
        # gemiddelde cijfer van de ratings van de tegenstanders.
        if (winst_12 == TRUE & som_12 > som_34) {
          resultaat_1 <- gemiddeld_34 - 1
          resultaat_2 <- gemiddeld_34 - 1
          resultaat_3 <- gemiddeld_12 + 1
          resultaat_4 <- gemiddeld_12 + 1
        }
        
        if (winst_12 == FALSE & som_12 < som_34) {
          resultaat_1 <- gemiddeld_34 + 1
          resultaat_2 <- gemiddeld_34 + 1
          resultaat_3 <- gemiddeld_12 - 1
          resultaat_4 <- gemiddeld_12 - 1
        }
        
        # Als de opgeteld sterkste combinatie wint levert de partij 
        # voor geen van de spelers een DSS resultaat op.
        if (winst_12 == TRUE & som_12 < som_34) {
          resultaat_1 <- NA
          resultaat_2 <- NA
          resultaat_3 <- NA
          resultaat_4 <- NA
        }
        
        if (winst_12 == FALSE & som_12 > som_34) {
          resultaat_1 <- NA
          resultaat_2 <- NA
          resultaat_3 <- NA
          resultaat_4 <- NA
        }
      }
    }
  }
  
  if (winst_12 != TRUE & winst_12 != FALSE) {
    resultaat_1 <- NA
    resultaat_2 <- NA
    resultaat_3 <- NA
    resultaat_4 <- NA
  }
  
  return(list("resultaat_1" = resultaat_1, 
              "resultaat_2" = resultaat_2, 
              "resultaat_3" = resultaat_3, 
              "resultaat_4" = resultaat_4))
}

# DSS_dubbel(rating_1 = 5.6093,
#            rating_2 = 4.6385,
#            rating_3 = 5.4011,
#            rating_4 = 5.8482,
#            winst_12 = TRUE)
