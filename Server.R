#
#title: "shiny Application for the "
#author: "vesr"
#date: "December 1, 2017"
#

# Load Libraries 
# 
library(shiny)
library(leaflet)
library(ggmap)
library(tableHTML)
library(xml2)
library(shinythemes)
library(rvest)
library(dplyr)
library(stringr)
library(RCurl)
library(lubridate)
library(curl)
library(ggplot2)


# Read Population Information from UN website

population <- read_html("http://bit.ly/29Qqzm3") %>% html_nodes("table") %>% .[[2]] %>% html_table(trim = TRUE)
#
# Identify columns needed for our analysis

population <-population[, c(2,3,4,5,6)]
names(population) <- c("country","region","statregion", "p2016", "p2017")
population <- mutate(population,
                     country = str_replace(country, "^[^[:alpha:]]*", ""),
                     country = iconv(country, to='ASCII//TRANSLIT'),
                     country = sub("Democratic People's Republic of Korea", "North Korea", country),
                     country = sub("Republic of Korea", "South Korea", country),
                     country = sub("Brunei Darussalam", "Brunei", country),
                     country = sub("Lao People's Democratic Republic", "Laos", country),
                     country = sub("Republic of Moldova", "Moldova", country),
                     country = sub("Russian Federation", "Russia", country),
                     country = sub("United Republic of Tanzania", "Tanzania", country),
                     country = sub("Viet Nam", "Vietnam", country)
)

# Data Clean-up removing superscript, subscripts etc

population[1] <- lapply(population[1], gsub, pattern = "[a]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[b]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[c]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[d]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[e]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[f]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[g]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[h]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[i]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[j]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[k]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[l]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[m]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[n]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[o]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[p]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[q]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[r]", replacement = "", fixed = TRUE)
population[1] <- lapply(population[1], gsub, pattern = "[s]", replacement = "", fixed = TRUE)

# Create Dataset for Country Drop-Down

c1 <-population[1]

# Life Expectancy
# Read Expectancy Details

expectancy <- read_html("http://bit.ly/1LjieDy") %>% html_nodes("table") %>% .[[1]] %>% html_table(trim = TRUE)
#
# Pick columns that are relavant for analysis
#
expectancy <- expectancy[, c(1, 5, 7)]
names(expectancy) <- c("country", "female", "male" )
expectancy <- mutate(expectancy,
                     country = str_replace(country, "^[^[:alpha:]]*", ""),
                     country = iconv(country, to='ASCII//TRANSLIT'),
                     country = sub("Democratic People's Republic of Korea", "North Korea", country),
                     country = sub("Republic of Korea", "South Korea", country),
                     country = sub("Brunei Darussalam", "Brunei", country),
                     country = sub("Lao People's Democratic Republic", "Laos", country),
                     country = sub("Republic of Moldova", "Moldova", country),
                     country = sub("Russian Federation", "Russia", country),
                     country = sub("United Republic of Tanzania", "Tanzania", country),
                     country = sub("Viet Nam", "Vietnam", country)
)

# Server Function
# Data Management and Computation 

server <- function(input, output) {
  
  # Setup the map (leafleft map details)
  output$map <- renderLeaflet({
    
    # Get latitude and longitude
    if(input$target_zone=="World"){
      ZOOM=2
      LAT=0
      LONG=0
    }else{
      target_pos=geocode(input$target_zone)
      LAT=target_pos$lat
      LONG=target_pos$lon
      ZOOM=5
    }
    
    # Intialize the Life Expectancy for the male and Female.  Averages have been initialized
    #
    ExpFemale <-80
    Expmale <- 79.6
    
    # Data Formating for the display purpose
    
    a<-subset(population$p2016,population$country == input$target_zone)
    b<-subset(population$p2017,population$country == input$target_zone)
    a1<-as.numeric(gsub(",", "",a))
    b1<-as.numeric(gsub(",", "",b))
    country<-input$target_zone             
    c <- round((b1-a1)/b1 * 100,2) * 5
    d<-  b1 + (b1 -a1)* 5
    if (input$target_zone == "World")
    {
      d<-7965526517
      
    }
    e<- formatC(d, format= "fg", big.mark=",")
    
    PPopup <- "Population Details"
    P2016D <- paste("1..   2016 Population: ", a)
    P2017D <- paste("2..   2017 Population: ", b)
    P2022D <- paste("3..   2023 Population:", e)
    CD     <- paste("4..   Change %: ", c)
    EPopup <- "Life Expectancy Details"
    
    # Read data table and find the matching values for the Life Expectancy
    #
    ExpFemale<-subset(expectancy$female, expectancy$country == input$target_zone)
    Expmale<-subset(expectancy$male, expectancy$country == input$target_zone)
    
    if (input$target_zone == "World")
    {
      ExpFemale <-"72"
      Expmale <-"70.6"
    }
    #ExpFemale<-subset(expectancy$female, expectancy$country == input$target_zone)
    #Expmale<-subset(expectancy$male, expectancy$country == input$target_zone)
    
    
    adjust <-length(ExpFemale)
    #   if ( adjust == 0 ) { ExpFemale <- "80" }
    
    #  if ( adjust == 0 ) { Expmale <- "79.6" }
    
    
    ExpFemaleDesc <- paste("1..   Life Expectancy Female: ", ExpFemale)
    ExpmaleDesc <- paste("2..   Life Expectancy Male: ", Expmale)
    
    # Create the content details for the popup dynamically
    #
    content <- paste(sep = "<br/>",  country)
    
    
    
    # Set initial Life Expectancy details (if the check box Life Expectancy is true)
    
    if (input$LifeExpectancy  )
    {
      content <- paste(sep = "<br/>",  country,
                       " <b> <u> Life Expectancy Projection </u> </b> ", 
                       ExpFemaleDesc, ExpmaleDesc
      )
    }
    
    # set initial Population Values (if the population check box is set to true)
    if (input$popprojection)
    {
      content <- paste(sep = "<br/>",  country,
                       " <b> <u> Population Projection </u> </b> ",
                       
                       P2016D, P2017D, P2022D, CD
                       
      )
    }
    
    # if both check boxes are selected - concatinate the details
    
    if (input$LifeExpectancy  & input$popprojection)
    {
      content <- paste(sep = "<br/>",  country,
                       " <b> <u> Population Projection </u> </b> ",
                       
                       P2016D, P2017D, P2022D, CD, 
                       " <b> <u> Life Expectancy Projection </u> </b> ", 
                       ExpFemaleDesc, ExpmaleDesc
      )
    }
    
    if (is.na(LONG) ) { LONG <- 20 }
    if (is.na(LAT) ) { LAT <- 20 }
    
    # Plot it!
   # LONG <- 0
  # LAT <- 0
    if (LONG > -360  & LAT > -360)
    {
    leaflet() %>% 
      addTiles() %>%
      setView(lng=LONG, lat=LAT, zoom=ZOOM ) %>%
      addProviderTiles("Esri.WorldImagery")  %>%
      addPopups(LONG, LAT, content,
                options = popupOptions(closeButton = FALSE)
      )
    }
  } )
}
shinyApp(ui = ui, server = server)