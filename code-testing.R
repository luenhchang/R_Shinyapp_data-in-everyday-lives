# 09-Dec-2024
# Code testing
# https://glin.github.io/reactable/articles/examples.html#custom-rendering-1

library(reactable)
reactable(iris)

# Create an interactive data table
## The columns argument in the reactable function is used to customize the appearance and behavior of specific columns
reactable(
  iris[1:5, ],
  columns = list(
    Sepal.Length = colDef(name = "Sepal Length"),
    Sepal.Width = colDef(name = "Sepal Width"),
    Species = colDef(align = "center")
  )
)

# For convenience, you can also specify a default colDef() to use for all columns in defaultColDef:
reactable(
  iris[1:5, ]
  ,defaultColDef = colDef(
    header = function(value) gsub(".", " ", value, fixed = TRUE)
    ,cell = function(value) format(value, nsmall = 1)
    ,align = "center"
    ,minWidth = 70
    ,headerStyle = list(background = "#f7f7f8")
    # Sort missing values last
    ,sortNALast = TRUE
    )
  ,columns = list(
    Species = colDef(minWidth = 140)  # overrides the default
  )
  ,bordered = TRUE
  ,highlight = TRUE
  ,defaultSorted = c("Species", "Petal.Length")
  ,showPagination = TRUE
  )

# Cell rendering
data <- MASS::Cars93[1:5, c("Manufacturer", "Model", "Type", "AirBags", "Price")]

reactable(data, columns = list(
  Model = colDef(cell = function(value, index) {
    # Render as a link
    url <- sprintf("https://wikipedia.org/wiki/%s_%s", data[index, "Manufacturer"], value)
    htmltools::tags$a(href = url, target = "_blank", as.character(value))
  }),
  AirBags = colDef(cell = function(value) {
    # Render as an X mark or check mark
    if (value == "None") "\u274c No" else "\u2714\ufe0f Yes"
  }),
  Price = colDef(cell = function(value) {
    # Render as currency
    paste0("$", format(value * 1000, big.mark = ","))
  })
))

# Embedding HTML widgets
library(dplyr)
library(sparkline)

data <- chickwts %>%
  group_by(feed) %>%
  summarise(weight = list(weight)) %>%
  mutate(boxplot = NA, sparkline = NA) # dim(data) 6 4

reactable(data, columns = list(
  weight = colDef(cell = function(values) {
    sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(chickwts$weight))
  }),
  boxplot = colDef(cell = function(value, index) {
    sparkline(data$weight[[index]], type = "box")
  }),
  sparkline = colDef(cell = function(value, index) {
    sparkline(data$weight[[index]])
  })
))

# To use reactable in Shiny apps, use renderReactable() and reactableOutput():
library(shiny)
library(reactable)

ui <- fluidPage(
  titlePanel("reactable example"),
  reactableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderReactable({
    #reactable(iris)
    reactable(data, columns = list(
      weight = colDef(cell = function(values) {
        sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(chickwts$weight))
      }),
      boxplot = colDef(cell = function(value, index) {
        sparkline(data$weight[[index]], type = "box")
      }),
      sparkline = colDef(cell = function(value, index) {
        sparkline(data$weight[[index]])
      })
    ))
    
  })
}

shinyApp(ui, server)
