#----------------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives/ui.R
# Date created: 13-Nov-2024
# Author(s): Lun-Hsien Chang
# Modified from: C:/GoogleDrive/scripts/R-shinyapp_data-in-everyday-lives/ui.R
# Dependency:
# Input:
## menuItem-About.html
# Output: https://luenhchang.shinyapps.io/barcode-scan/
# References
## [Horizontal Rule hr() in R Shiny Sidebar](https://stackoverflow.com/questions/43592163/horizontal-rule-hr-in-r-shiny-sidebar)
## Date       Changes:
##---------------------------------------------------------------------------------------------------------------
## 2025-01-13 Deleted code for menuItem "Fitness", "Data Challenges"
## 2024-06-13 Removed menuItem Jobs
## 2024-05-16 Deployed app. App is launched faster in phone and all valueBoxes and infoBoxes working.
## 2024-05-16 Replaced in server.R shiny::renderUI({rmarkdown::render() shiny::includeHTML('menuItem-About.html')}) and uiOutput() in ui.R with includeHTML(path="html-file-path") in ui.R. The html file knitted in Rmd following [Displaying html file using includeHTML in shiny is not working with renderUI()](https://stackoverflow.com/questions/56064805/displaying-html-file-using-includehtml-in-shiny-is-not-working-with-renderui). The problem is that knitting an .Rmd file creates an HTML document with <html><head><title><body> etc. while fluidPage() does exactly the same. So including a complete HTML document into fluidPage() creates problems due to redundancy. Fortunately, there's a very easy solution: use output: html_fragment in the YAML header of your .Rmd file before knitting and saving it as .html document.   
## 2024-05-07 Added menuItem "Jobs"
## 2024-05-02 Placed slickROutput() inside box()
##---------------------------------------------------------------------------------------------------------------

#---------------------------------
# Webpage title on top left corner
#---------------------------------
#application.title <- "Data in everyday lives"
application.title <- tags$a(href="#top" # A destination to go to. Can be a URL or locations in this app 
                            ,icon('chart-line')
                            ,"Data in everyday lives")

#--------------------------------------------
# Define dashboardPage() function components
## Reference [https://stackoverflow.com/questions/67237358/set-font-size-and-color-in-title-of-shinydashboard-box](https://stackoverflow.com/questions/67237358/set-font-size-and-color-in-title-of-shinydashboard-box)
## [R-Shiny-Dashboards/USArrestDashboard/ui.R](https://github.com/aagarw30/R-Shiny-Dashboards/blob/main/USArrestDashboard/ui.R)
#--------------------------------------------
header <- shinydashboard::dashboardHeader(
  title = application.title
  ,titleWidth = 650
  ,tags$li(class="dropdown"
           ,tags$a(href="https://www.linkedin.com/in/lunhsienchang/", icon("linkedin", "My profile", target="_blank")))
  )

sidebar <- shinydashboard::dashboardSidebar(
  width = 200
  ,shinydashboard::sidebarMenu(
    # Change font size to 30
    ## Reference [shinydashboard: change font size of menuItem in sidebarMenu [duplicate]](https://stackoverflow.com/questions/53559195/shinydashboard-change-font-size-of-menuitem-in-sidebarmenu)
    tags$style(HTML(".sidebar-menu li a { font-size: 20px; }"))
    ,shinydashboard::menuItem(text = "Electricity", tabName = "tabElectricity", icon = icon("chart-line"))
    ,shinydashboard::menuItem(text = "About", tabName = "tabAbout",icon = icon("home",lib = "glyphicon"))
    ,shinydashboard::menuItem(text = "Food", tabName = "tabPackagedFood", icon = icon("chart-line"))
    ,shinydashboard::menuItem(text = "Bathroom", tabName = "tabHygieneProducts", icon = icon("chart-line"))
    ,shinydashboard::menuItem(text = "Recycling", tabName = "tabContainers", icon = icon("chart-line"))
    ,shinydashboard::menuItem(text = "Employment", tabName = "tabEmployment", icon = icon("chart-line"))
    )
)

# Level 1 header style
style.header <- "text-align: left; padding-bottom: 10px; 
                 font-family: 'Roboto', sans-serif; 
                 font-weight: bold; 
                 color: #000000;"  # Pure black without text shadow

body <- shinydashboard::dashboardBody(
  shinydashboard::tabItems(
    #************************************
    # menuItem "Electricity"
    #************************************
    shinydashboard::tabItem(
      tabName = "tabElectricity"
      ,fluidRow(
        shinydashboard::valueBoxOutput(outputId = "valueBox.amount.paid.electricity.consumption.total.breakdown", width = 5)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.electricity.consumption.total", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.solar.export.total", width = 2)
      ) # Close fluidRow
      ,fluidRow(
        box(title="Electricity usage and solar export"
            ,status="primary"
            ,solidHeader=TRUE
            ,width = 12 # By default box is set to width = 6. full width = 12
            ,height = 455 # Plot can go outside box border if height too small # White space if height too big
            ,plotOutput(outputId="plot.energy.usage.solar.export"))
      ) # Close fluidRow
      ,fluidRow(
        box(title = "Alinta energy bill- balance brought forward"
            ,status = "primary"
            ,solidHeader = TRUE
            ,width = 12
            ,DTOutput(outputId="table.electricity.usage.solar.export")
        )
      ) # Close fluidRow
      ,fluidRow(
        box(
          title = shiny::HTML(
            "Alinta energy bill - supply charges and rates. All rates listed per KWh and included GST")
          ,status = "primary"
          ,solidHeader = TRUE
          ,width = 12
          ,DTOutput(outputId="table.rates.over.supply.period")
        ) # Close box()
      ) # Close fluidRow
    ) # Close tabItem() for menuItem "Electricity"
    
    #************************************
    # menuItem "About"
    #************************************
    ,shinydashboard::tabItem(
      tabName = "tabAbout"
      ,fluidRow(
        box(
          title=""
          ,includeHTML(path=file.path(getwd(),"menuItem-About.html"))
          ,width=NULL)
      )
    )
    #************************************
    # menuItem "Food"
    #************************************
    ,shinydashboard::tabItem(
      tabName = "tabPackagedFood"
      ,fluidRow(
        # valueBox 1 to 5
         shinydashboard::valueBoxOutput(outputId = "valueBox.num.unique.food.barcodes", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.num.food.category", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.num.food.no.category", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.num.food.expired", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.summed.price.food", width = 3)
        ) # Close fluidRow
      ,fluidRow(
        box(title="Proportions of food categories"
            ,status = "primary"
            ,solidHeader = TRUE
            ,width = 7
            ,plotOutput(outputId = "plot.treemap.food.category"))
        ,box(title = "Food category"
             ,status = "primary"
             ,solidHeader = TRUE
             ,collapsible = TRUE
             ,width = 5
             ,DTOutput(outputId="table.food.category.product.name"))
        ) # Close fluidRow
      ,fluidRow(
        box(title="Expiry of foods"
            ,status = "primary"
            ,solidHeader = TRUE
            ,collapsible = TRUE
            ,width = 5
            ,DTOutput(outputId="table.food.expiring.DT", width = "100%", height = "100%"))
        ,box(title = "Food usage"
             ,status = "primary"
             ,solidHeader = TRUE
             ,collapsible = TRUE
             ,width = 7
             ,DTOutput(outputId="table.food.consumed.DT", width = "100%", height = "100%"))
        ) # Close fluidRow
      ,fluidRow(
        box(title = "Food prices"
            ,status = "primary"
            ,solidHeader = TRUE
            ,collapsible = TRUE
            ,width = 7
            ,DTOutput(outputId="table.food.price.DT", width = "100%", height = "100%")) # Close box()
          ) # Close fluidRow
      ,fluidRow(
        box(title="Monthly spend on food categories-interactive plot using plotly package"
             ,status = "primary"
             ,solidHeader = TRUE
             ,width = 12
             #,height = 20 # didn't increase height
             ,plotly::plotlyOutput(outputId="plotly.barplot.monthly.spend.food.category",width = "100%", height = "100%"))
        ) # Close fluidRow
      ,fluidRow(
        # dynamic inforBox 1
        infoBoxOutput(outputId = "infoBox.food.barcode.earliest.date.record", width = 3)
        # dynamic inforBox 2
        ,infoBoxOutput(outputId = "infoBox.food.barcode.latest.date.record", width = 3)
        ) # Close fluidRow
    ) # Close tabItem() for menuItem "Food"
    
    #*****************************************
    # menuItem "Bathroom"
    #*****************************************
    ,shinydashboard::tabItem(
      tabName = "tabHygieneProducts"
      ,fluidRow(
        # valueBox 1
        shinydashboard::valueBoxOutput(outputId = "valueBox.num.unique.hygiene.barcode", width = 2)
        # valueBox 2
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.summed.price.hygiene", width = 2)
        ) # Close fluidRow
      ,fluidRow(
        box(title = "Personal care product in stock"
            ,status = "primary"
            ,solidHeader = TRUE
            ,collapsible = TRUE
            ,DTOutput(outputId="table.hygiene.count.unopened.DT"))
        ,box(title="Plot 2.1"
             ,status = "primary"
             ,solidHeader = TRUE
             ,"Create a plot using table 2.1 data", br(), "A different way to visualise table")
        ) # Close fluidRow
      ,fluidRow(
        box(title = "Personal care product prices"
            ,status = "primary"
            ,solidHeader = TRUE
            ,collapsible = TRUE
            ,DTOutput(outputId="table.hygiene.price.DT")
            ) # Close box
        ,box(title="Plot 2.2"
             ,status = "primary"
             ,solidHeader = TRUE
             ,"Create a plot using table 2.2 data", br(), "A different way to visualise table"
             ) # Close box
        ) # Close fluidRow
    ) # Close tabItem() for menuItem "Bathroom"
    
    #*****************************************
    # menuItem "Recycling"
    #*****************************************
    ,shinydashboard::tabItem(
      tabName = "tabContainers"
    # Year to date (current)
      ,fluidRow(
        shinydashboard::valueBoxOutput(outputId = "valueBox.numb.all.containers.stock", width = 3)
      ) # Close fluidRow
    # 2025
      ,fluidRow(
        # Add a title row left-aligned
        column(
          width = 12,
          tags$h1("This Year's Container Recycling", style = style.header)
        )
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.year.in.container.collection.2025", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.numb.collections.made.2025", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.numb.containers.collected.2025", width = 3)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.numb.refunds.received.2025", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.numb.containers.refunded.2025", width = 3)
      ) # Close fluidRow
      ,fluidRow(
        box(title="Container collection and refund in 2025"
            ,status="primary"
            ,solidHeader=TRUE
            ,width = 12 # By default box is set to width = 6. full width = 12
            ,height = 455 # Plot can go outside box border if height too small # White space if height too big
            ,plotlyOutput(outputId="plot.stacked.bars.containers.2025"))
      ) # Close fluidRow
    # 2024
      ,fluidRow(
        # Add a title row left-aligned
        column(
          width = 12,
          tags$h1("Pass Years' Container Recycling", style = style.header)
        )
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.year.in.container.collection.2024", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.numb.collections.made.2024", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.numb.containers.collected.2024", width = 3)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.numb.refunds.received.2024", width = 2)
        ,shinydashboard::valueBoxOutput(outputId = "valueBox.numb.containers.refunded.2024", width = 3)
    ) # Close fluidRow
    
      ,fluidRow(
        box(title="Container collection and refund in 2024"
            ,status="primary"
            ,solidHeader=TRUE
            ,width = 12 # By default box is set to width = 6. full width = 12
            ,height = 455 # Plot can go outside box border if height too small # White space if height too big
            ,plotOutput(outputId="plot.stacked.bars.containers.2024"))
        ) # Close fluidRow
      ,fluidRow(
        box(title = "Number of containers collected or refunded per day"
            ,status = "primary"
            ,solidHeader = TRUE
            ,width = 12
            ,DTOutput(outputId="table.containers.daily.DT")
            )
      ) # Close fluidRow
      ,fluidRow(
         shinydashboard::infoBoxOutput(outputId = "infoBox.date.earliest.record.recycling",width = 3)
        ,shinydashboard::infoBoxOutput(outputId ="infoBox.date.latest.record.recycling" ,width = 3)
      )
    ) # Close tabItem() for menuItem "Recycling"
    #************************************
    # menuItem "Employment"
    #************************************
    ,shinydashboard::tabItem(
       tabName = "tabEmployment"
      ,fluidRow(
        # Add a title row left-aligned
        column(
          width = 12,
          tags$h1("Employment Timeline", style = style.header)
          )
      ) # Close fluidRow()
      ,fluidRow(
        box(title="Employment events"
            ,status="primary"
            ,solidHeader=TRUE
            ,width = 12 # By default box is set to width = 6. full width = 12
            ,height = 455 # Plot can go outside box border if height too small # White space if height too big
            ,plotlyOutput(outputId="plot.employment.horizontal.bars"))
      ) # Close fluidRow
      ,fluidRow(
        # Add a title row left-aligned
        column(
          width = 12,
          tags$h1("Job application events", style = style.header)
        )
      ) # Close fluidRow()
      ,fluidRow(
        box(title="Job application events"
            ,status="primary"
            ,solidHeader=TRUE
            ,width = 12 # By default box is set to width = 6. full width = 12
            ,plotlyOutput( outputId="plot.job.application.horizontal.bars"
                          ,height = "600px"   # Set height of the plot here
                          )
        ) # Close box()
      ) # Close fluidRow
      ) # Close tabItem() for menuItem "Employment"
  ) # Close tabItems
) # Close dashboardBody()

#-------------------------------------------------------------------------------------
# User interface by shinydashboard
## The dashboardPage() function expects three components: a header, sidebar, and body:
## References [shinydashboard](https://rstudio.github.io/shinydashboard/index.html)
#-------------------------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(
  title = "Everyday Data" # A title to display in the browser's title bar. If no value is provided, it will try to extract the title from the dashboardHeader.
  #title = span(tagList(icon("calendar")),"Everyday Data")
  ,header=header
  ,sidebar=sidebar
  ,body=body
  ,skin = "black")

#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#