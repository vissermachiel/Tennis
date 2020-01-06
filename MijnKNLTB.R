# R script to browse tennis player profiles at KNLTB website
# Machiel Visser
# 13 October 2019

# Clean workspace
# rm(list = ls())
rm(list = setdiff(ls(), c("rD", "remDr")))

# Load packages
library(RSelenium)
library(ggplot2)

# Set parameters
knltb_nr <- "15860612"
seizoen <- "2019"
webElem2 <- NULL

# Start a selenium server and browser
rD <- rsDriver(port = 3333L, browser = "firefox")
remDr <- rD[["client"]]
remDr$maxWindowSize()
# remDr$setWindowPosition(x = -5, y = 0)
# remDr$setWindowSize(width = 778, height = 829)

# Navigate to website
remDr$navigate("https://mijnknltb.toernooi.nl")

# Accept cookies, if pop-up appears
suppressMessages(try(remDr$findElement(using = "xpath", value = '/html/body/div/div/div/div/div[2]/footer[1]/p/a')$clickElement(), silent = TRUE))

# Search for player by knltb number
remDr$findElement(using = "xpath", value = '//*[@id="MastheadSearchInput"]')$sendKeysToElement(list(knltb_nr, key = "enter"))

# Wait till player is found and open player's profile
while(is.null(webElem2)){
  webElem2 <- tryCatch(suppressMessages(remDr$findElement(using = 'xpath', value = '/html/body/div[1]/div[1]/div[1]/div/div[2]/div[3]/div/ul/li[3]/div/div/ul/div/div/div/h5/a/span')), error = function(e) {NULL})
}
webElem2$clickElement()

# Player name
player <- unlist(remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[3]/div[1]/div/div/div[2]/h2/span[1]/span')$getElementText())

# Player's rating page
# remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[3]/div[2]/ul/li[5]/a')$clickElement()
remDr$navigate(paste0(remDr$getCurrentUrl()[[1]], "/Rating"))

# Select playing season
if(seizoen == "2018") {
  remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[1]/div/ul/li[3]')$clickElement()
  remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[1]/div/ul/li[3]/div')$clickElement()
}

if(seizoen == "2019") {
  remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[1]/div/ul/li[2]')$clickElement()
}

if(seizoen == "2020") {
  remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[1]/div/ul/li[1]')$clickElement()
}

#### Singles

# End of year rating singles
eindejaars_enkel <- as.numeric(gsub(",", ".", remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[2]/div[1]/div[1]/div/div/div[1]/ul/li[2]/div/div/span[2]')$getElementText()))

# Number of singles matches
n_enkel <- as.numeric(remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[2]/div[1]/div[1]/div/div/div[1]/ul/li[3]/div/div/span[2]')$getElementText())

# Show singles matches
remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[2]/div[1]/div[1]/button')$clickElement()

# Results from singles matches
resultaten_enkel <- rep(NA, n_enkel)

## Green - Match won  - "rgb(74, 191, 51)"
## Red   - Match lost - "rgb(189, 59, 42)"
## Grey  - No results - "rgb(112, 117, 133)"
## Grey  - Time stamp - "rgb(225, 226, 229)"

for (i in 1:n_enkel) {
  try({
    foo <- suppressMessages(remDr$findElement(using = "xpath", value = paste0('/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[2]/div[1]/div[2]/div/ol/li[', i, ']/div/div[1]/ul/li')))
    foo_col <- unlist(foo$getElementValueOfCssProperty(propName = "background-color"))
    if (foo_col == "rgb(74, 191, 51)" | foo_col == "rgb(189, 59, 42)") {
      resultaten_enkel[i] <- as.numeric(gsub(",", ".", foo$getElementText()))
    }
    if (foo_col == "rgb(225, 226, 229)") {
      foo <- remDr$findElement(using = "xpath", value = paste0('/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[2]/div[1]/div[2]/div/ol/li[', i, ']/div/div[1]/ul/li[2]'))
      foo_col <- unlist(foo$getElementValueOfCssProperty(propName = "background-color"))
      if (foo_col == "rgb(74, 191, 51)" | foo_col == "rgb(189, 59, 42)") {
        resultaten_enkel[i] <- as.numeric(gsub(",", ".", foo$getElementText()))
      }
    }
  }, silent = TRUE)
}

resultaten_enkel <- rev(resultaten_enkel)

# Function to calculate new rating
Rating <- function(resultaten, eindejaars) {
  if(sum(!is.na(resultaten)) >= 6) {
    actuele_rating <- mean(resultaten, na.rm = TRUE)
  } else {
    actuele_rating <- (sum(resultaten, na.rm = TRUE) + (6 - sum(!is.na(resultaten))) * eindejaars) / 6
  }
  return(round(actuele_rating, 4))
}

# Calculate new rating after each match
rating_enkel <- numeric(n_enkel)

for (i in 1:n_enkel) {
  rating_enkel[i] <- Rating(resultaten_enkel[1:i], eindejaars_enkel)
}

#### Doubles

# End of year rating doubles
eindejaars_dubbel <- as.numeric(gsub(",", ".", remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[2]/div[2]/div[1]/div/div/div[1]/ul/li[2]/div/div/span[2]')$getElementText()))

# Number of doubles matches
n_dubbel <- as.numeric(remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[2]/div[2]/div[1]/div/div/div[1]/ul/li[3]/div/div/span[2]')$getElementText())

# Show doubles matches
remDr$findElement(using = "xpath", value = '/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[2]/div[2]/div[1]/button')$clickElement()

# Results from doubles matches
resultaten_dubbel <- rep(NA, n_dubbel)

for (i in 1:n_dubbel) {
  try({
    foo <- suppressMessages(remDr$findElement(using = "xpath", value = paste0('/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[2]/div[2]/div[2]/div/ol/li[', i, ']/div/div[1]/ul/li')))
    foo_col <- unlist(foo$getElementValueOfCssProperty(propName = "background-color"))
    if (foo_col == "rgb(74, 191, 51)" | foo_col == "rgb(189, 59, 42)") {
      resultaten_dubbel[i] <- as.numeric(gsub(",", ".", foo$getElementText()))
    }
    if (foo_col == "rgb(225, 226, 229)") {
      foo <- remDr$findElement(using = "xpath", value = paste0('/html/body/div[1]/div[4]/div[2]/div/div/div/div[2]/div[2]/div[2]/div[2]/div/ol/li[', i, ']/div/div[1]/ul/li[2]'))
      foo_col <- unlist(foo$getElementValueOfCssProperty(propName = "background-color"))
      if (foo_col == "rgb(74, 191, 51)" | foo_col == "rgb(189, 59, 42)") {
        resultaten_dubbel[i] <- as.numeric(gsub(",", ".", foo$getElementText()))
      }
    }
  }, silent = TRUE)
}

resultaten_dubbel <- rev(resultaten_dubbel)

# Calculate new rating after each match
rating_dubbel <- numeric(n_dubbel)

for (i in 1:n_dubbel) {
  rating_dubbel[i] <- Rating(resultaten_dubbel[1:i], eindejaars_dubbel)
}

# Put everything in a dataframe
wedstrijden_df <- data.frame(Wedstrijden = c(0:length(rating_enkel),
                                             0:length(rating_dubbel)),
                             Speltype = c(rep("Enkel", 1 + length(rating_enkel)),
                                          rep("Dubbel", 1 + length(rating_dubbel))),
                             Rating = c(eindejaars_enkel, rating_enkel,
                                        eindejaars_dubbel, rating_dubbel))

# Plot change in rating during season
ggsave(paste0("Rating_", knltb_nr, "_", seizoen, ".png"),
       ggplot(wedstrijden_df, aes(x = Wedstrijden, y = Rating, group = Speltype, colour = Speltype)) +
         geom_point(size = 2) +
         geom_line(size = 1) +
         ggtitle(paste("Speelsterkte", player, seizoen)) +
         xlab("Wedstrijden") + 
         ylab("Rating") +
         theme(plot.title = element_text(hjust = 0.5),
               text = element_text(size = 16)),
       width = 200, height = 200, units = "mm")

#### END ####