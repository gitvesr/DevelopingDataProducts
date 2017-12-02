#
#title: "shiny Application for the "
#author: "vesr"
#date: "December 1, 2017"
#



# User interface page with the reactive controls
#

ui <- fluidPage(
  
  tags$h2("Data Products Assignment: Population and Life Expectancy Projection"),
  tags$h4("(Select country name from drop-down: Filter by Life Expectancy/Population)"),
  # titlePanel("Data Products Assignment: Population and Life Expectancy Projection"),
  
  leafletOutput("map", height="550px"),
  #absolutePanel(top=20, left=70, textInput("target_zone", "" , "Ex: Bamako")),
  checkboxInput("LifeExpectancy", "Life Expectancy", TRUE),
  checkboxInput("popprojection", "Population Projection", TRUE),
  absolutePanel(top=89, left=90, selectInput("target_zone", "Country" , c1)),
  
  #span(textOuput("message"), style="color:red")
  
  br()
)

# Apply Shiny App
shinyApp(ui = ui, server = server)