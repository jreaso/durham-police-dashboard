library("shiny")
library("tidyverse")
library("ukpolice")
library("leaflet")
library("ggplot2")

nbd <- ukc_neighbourhoods("durham")
nbd2 <- nbd$id
print(nbd2)
names(nbd2) <- nbd$name

# Application UI
ui <- fluidPage(
  titlePanel("Durham Police Data Dashboard"),
  
  HTML("This dashboard displays crime data for different constituencies under Durham Police by month periods. The data used is provided by UK Police API (<a href='https://data.police.uk/docs/' target='_blank'>data.police.uk</a>) via the <code>ukpolice</code> package. The map is interactive and shows the location of each crime with the category."),
  
  fluidRow(
    column(
      width = 3,
      offset = 3,
      selectInput("nbd", "Durham Constabulary", choices = nbd2, selected = "DHAM1")
    ),
    column(
      width = 3,
      textInput("date", "Month Period (YYYY-MM)", value = "2020-08")
    )
  ),
  
  fluidRow(
    column(
      width = 7,
      h3("Crime Counts Chart"),
      plotOutput("barchart"),
    ),
    column(
      width = 5,
      h3("Neighbourhood Map"),
      leafletOutput("map", width = "100%") # Make the map full width
    )
  ),
  
  div(
    style = "text-align: center; padding: 10px; position: fixed; bottom: 0; left: 0; right: 0; background-color: #f5f5f5;",
    p("Copyright © 2023 Jamie Reason")
  )
  
)


# Application Server for charts and maps
server <- function(input, output) {
  bdy <- reactive({
    ukc_neighbourhood_boundary("durham", input$nbd) |> 
      mutate(latitude = as.numeric(latitude),
             longitude = as.numeric(longitude))
  })
  
  crimes <- reactive({  
    bdy2 <- bdy() |> 
      select(lat = latitude,
             lng = longitude)
    
    ukc_crime_poly(bdy2[round(seq(1, nrow(bdy2), length.out = 100)), ], input$date)
  })
  
  output$barchart <- renderPlot({
    ggplot(crimes()) +
      geom_bar(aes(y = category, fill = outcome_status_category)) +
      labs(y = "Crime", fill = "Outcome Status")
  }, res = 96)
  
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |> 
      addPolygons(lng = bdy()$longitude, lat = bdy()$latitude) |> 
      addCircles(lng = as.numeric(crimes()$longitude), lat = as.numeric(crimes()$latitude), label = crimes()$category, color = "red") 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
