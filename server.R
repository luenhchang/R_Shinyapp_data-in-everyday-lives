#---------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive/scripts/R-shinyapp_data-in-everyday-lives/server.R
# Date created: 18-Apr-2024
# Author(s): Lun-Hsien Chang
# Modified from: C:/GoogleDrive/barcode-scanner/scripts/barcode-scanner_shiny-web-app.R
# Dependency:
# Local input: C:/GoogleDrive/scripts/R-shinyapp_data-in-everyday-lives/www/speed-test-printscreen_Telstra5G/*.jpg
## [Tree Maps Understanding and using Tree Maps](https://www.tableau.com/data-insights/reference-library/visual-analytics/charts/treemaps)
## Date       Changes:
##--------------------------------------------------------------------------------------------------------
## 2024-06-04 Added plotly bar plot subplots, one per food category. Currently no control on bar color and subplot titles
## 2024-05-16 Commented out shiny::renderUI({rmarkdown::render() shiny::includeHTML('menuItem-Fitness.html') }). Now includeHTML() is used to read a html file, which was knitted as html_fragment in .Rmd files
## 2024-05-07 Added a linebreak in renderDataTable with 2 steps: (1) add <br> as linebreak symbol, (2) set datatable(escape=FALSE)
## 2024-05-06 Used valueBox(subtitle=HTML(paste("<b>","text to bold","</b>")) to bold text in valueBox subtitle.
## 2024-05-06 Used valueBox(subtitle=HTML(paste(br()))) to add a new line in valueBox subtitle. 
## 2024-04-24 Created function.renderValueBox(), function.renderInfoBox() with optional arguments that have default values to generate script for renderValueBox(), renderInfoBox()
## 2024-04-23 Added a image slideshow under menuItem Data 
##--------------------------------------------------------------------------------------------------------------

# Color
color.global.infoBox <- "olive"
color.global.valueBox <- "orange"

server <- function(input, output, session) {
  
  # Stop running shiny app when closing browser window
  ## [How to stop running shiny app by closing the browser window?](https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window)
  ## This is a bad idea! If multiple users are connected to the app, then one user (or browser tab) exiting will cause everyone to be kicked off! – Joe Cheng Aug 7, 2020 at 19:23
  session$onSessionEnded(function() { stopApp() })
  
  #------------------------------------------------------------------------------------------------
  # Create functions for output
  ## [Create Function for output in Shiny](https://stackoverflow.com/questions/53590526/create-function-for-output-in-shiny)
  ## ["Correct" way to specifiy optional arguments in R functions](https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions)
  ## [How do you use "<<-" (scoping assignment) in R?](https://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r)
  #------------------------------------------------------------------------------------------------
  # Define function for renderValueBox()
  function.renderValueBox <- function(output.id
                                      ,argument.value
                                      ,argument.subtitle
                                      ,argument.icon
                                      ,argument.icon.lib
                                      ,argument.color){
    # Write default values to optional arguments
    if(missing(argument.icon)){argument.icon <- "th-list"}
    if(missing(argument.icon.lib)){argument.icon.lib <- "glyphicon"}
    if(missing(argument.color)){argument.color<-"orange"}
    
    output[[output.id]] <<- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = argument.value
        ,subtitle = argument.subtitle
        #,icon = icon(argument.icon, lib = argument.icon.lib)
        ,color = argument.color)
    }) # Close renderValueBox()
  } # Close function{}
  
  # Define function for renderInfoBox()
  function.renderInfoBox <- function(output.id, arg.title, arg.value, arg.icon, arg.color, arg.fill){
    # Write default values to optional arguments
    if(missing(arg.icon)){arg.icon<-"list"}
    if(missing(arg.color)){arg.color<-"olive"}
    if(missing(arg.fill)){arg.fill <- TRUE}

    output[[output.id]] <<- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        title = arg.title
        ,value = arg.value
        #,icon=icon(arg.icon)
        ,color = arg.color
        ,fill = arg.fill)
    }) # Close renderInfoBox()
  }# Close function{}
  
  #*****************************************
  # Outputs to use under menuItem "About"
  #*****************************************
  
  #***************************************************
  # Outputs to use under menuItem "Data Challenges"
  ## Image slide show
  ## [How to use multiple slickROutput in shiny dashboard](https://stackoverflow.com/questions/60605258/how-to-use-multiple-slickroutput-in-shiny-dashboard)
  ## [How can I render a Carousel in R Shiny that has image and text with SlickR?](https://stackoverflow.com/questions/77991364/how-can-i-render-a-carousel-in-r-shiny-that-has-image-and-text-with-slickr)
  ## [Dynamic Image Carousel R Shiny](https://stackoverflow.com/questions/69817284/dynamic-image-carousel-r-shiny)
  #***************************************************
  output$image.slide.show <- slickR::renderSlickR({
    # This works locally but it uses a local directory
    #x <- slickR(obj = list.files(path = dir.www, full.names=TRUE), height=500, slideId = "slick1")
    
    # This works locally
    x<- slickR::slickR(obj = list.files(path = paste0(getwd(),"/www/image_data-challenges-annotated"),pattern = ".jpg", full.names=TRUE), height=500, slideId = "slick1")
    y<- slickR::slickR(obj = c("nested-folders-subfolders[1]annotated.jpg","alphabetically-order-text-string.jpg"), slideType = "p", slideId = "slick2")
    # Display image and its file name at the same time
    y %synch% x
  })
  
  #*****************************************
  # Outputs to use under menuItem "Food"
  #*****************************************
  
  #----------------
  # Output valueBox
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #----------------
  function.renderValueBox(output.id="valueBox.num.unique.food.barcodes"
                          ,argument.value=num.unique.food.barcode
                          ,argument.subtitle="Barcodes scanned")
  
  function.renderValueBox(output.id="valueBox.num.food.category"
                          ,argument.value=num.food.category
                          ,argument.subtitle="Food categories")
  
  function.renderValueBox(output.id = "valueBox.num.food.no.category"
                          ,argument.value = num.food.no.category
                          ,argument.subtitle = "Uncategorised foods"
                          ,argument.icon ="thumbs-down" )
  
  function.renderValueBox(output.id="valueBox.num.food.expired"
                          ,argument.value=num.food.expired
                          ,argument.subtitle="Foods expired"
                          ,argument.icon ="thumbs-down" )
  
  function.renderValueBox(output.id="valueBox.summed.price.food"
                          ,argument.value=paste0("$AUD ",summed.price.food)
                          ,argument.subtitle="Spent on food"
                          ,argument.icon ="credit-card")

  #----------------------------------------------------------------------------------------
  # Output DT dataTable
  ## Ref [How can I introduce a new line within a column using DTedit and shiny::uiOutput?](https://stackoverflow.com/questions/56590555/how-can-i-introduce-a-new-line-within-a-column-using-dtedit-and-shinyuioutput)
  #----------------------------------------------------------------------------------------
  output$table.food.category.product.name <- DT::renderDataTable({
    table.food.category.product.name <- barcode.food.category |> 
      dplyr::select(category, product.name) |> 
      dplyr::distinct() |> 
      dplyr::arrange(category, product.name) |>
      dplyr::rename(Category=category, Product=product.name) # dim(table.food.category.product.name) 122 2
    
    DT::datatable(table.food.category.product.name
                  ,options = list(autoWidth = FALSE, searching = TRUE))
  })
  output$table.food.expiring.DT <- DT::renderDataTable({
    #print(food.expiring.365)
    DT::datatable(food.expiring.365
                  ,options = list(autoWidth = FALSE, searching = TRUE))
    
  }) # Close the renderDataTable function
  
  output$table.food.consumed.DT <- DT::renderDataTable({
    #print(food.consumed)
    DT::datatable(food.consumed
                  ,options = list(autoWidth=FALSE, searching=TRUE ))
  }) # Close the renderDataTable function
  
  output$table.food.price.DT <- DT::renderDataTable({
    #print(food.price)
    DT::datatable(food.price
                  ,options = list(autoWidth=FALSE, searching=TRUE ))
  }) # Close the renderDataTable function
  
  #-------------
  # Output plots 
  #-------------
  # Treemap for food category frequencies
  output$plot.treemap.food.category <- shiny::renderPlot({
    plot.treemap.food.category <- ggplot2::ggplot(data = barcode.food.category.count
                                                  ,aes(area=count, fill=count, label=category))+
      treemapify::geom_treemap()+
      treemapify::geom_treemap_text(colour="white"
                                    ,place = "centre"
                                    ,size=15
                                    ,grow = TRUE)+
      ggplot2::scale_fill_viridis_c()
    return(plot.treemap.food.category)
  }) # Close renderPlot()
  
  # Facet bar plots for monthly spend on food categories using ggplot2
  ## This plot is not showing up in R Shiny. Warning: Error in graphics::plot.new: figure margins too large
  output$ggplot2.barplot.monthly.spend.food.category <- shiny::renderPlot({
    
    # Monthly total expenditure on food category
    ## [How do I get R to recognize the appropriate order for Month Year combinations without manually specifying the order?](https://stackoverflow.com/questions/71498479/how-do-i-get-r-to-recognize-the-appropriate-order-for-month-year-combinations-wi)
    ## [Removing white spaces between facets when using ggplotly()](https://stackoverflow.com/questions/63152833/removing-white-spaces-between-facets-when-using-ggplotly)
    bar.plot.monthly.spend.on.food.category <- ggplot2::ggplot(
      data = expenditure.food.category
      ,aes(x= reorder(timestamp.month.label, lubridate::my(timestamp.month.label))
           ,y=price.summed
           ,text=paste("$AUD",price.summed,"spent on", category, "in", timestamp.month.label, timestamp.year))) +
      ggplot2::geom_col()+
      ggplot2::facet_wrap(~category, ncol = 4, scales = "free_y",strip.position = "top") +
      ggplot2::scale_x_discrete(expand = c(0,0))+
      ggplot2::labs(x="Month",y="Spend ($AUD)")+
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 10)
      )
    # Not converting a ggplot2 to a plotly object as white space between sub plots/facetted plots is not able to remove
    #plotly::ggplotly(p=bar.plot.monthly.spend.on.food.category, tooltip = "text")
  })
  
  # Facet bar plots for monthly spend on food categories using plotly
  ## [How to facet a plot_ly() chart?](https://stackoverflow.com/questions/58103196/how-to-facet-a-plot-ly-chart)
  ## ["Warning: Error in : Tibble columns must have compatible sizes." when adding hoverinfo to filtered plot](https://stackoverflow.com/questions/68291943/warning-error-in-tibble-columns-must-have-compatible-sizes-when-adding-hov)
  output$plotly.barplot.monthly.spend.food.category <- plotly::renderPlotly({
    expenditure.food.category |>
      dplyr::group_by(category.factor) |>
      plotly::do(p=plot_ly(.
                           , x=~timestamp.month.label
                           , y=~price.summed
                           , color = ~category.factor
                           , type = "bar"
                           , mode = "markers"
                           , orientation="v"
                           , text= ~price.summed # text placed on top of bars or as hoverinfo
                           , textposition="auto"
                           , hoverinfo="text"
                           , hovertext= ~paste(
                             "Month year :", timestamp.month.label, timestamp.year
                             ,"<br> Category :", category
                             ,"<br> Cost : $AUD", price.summed)
                           ) # Close plot_ly()
                 ) |>
      plotly::subplot(nrows = 4
                      ,shareX = TRUE 
                      ,shareY = FALSE
                      ,margin=0.01
                      ,titleX = FALSE
                      ,titleY = FALSE) 
  })
  
  # Facet lollipop plot using ggplot2
  # Lollipop chart horizontal. Lollipops overlap legend in vertical lollipop plot
  ## This plot is not showing up in R Shiny. Warning: Error in graphics::plot.new: figure margins too large
  ## [Lollipop chart with multiple groupings: how to remove and alter legends?](https://stackoverflow.com/questions/62391894/lollipop-chart-with-multiple-groupings-how-to-remove-and-alter-legends)
  ## [Text labels on condition for lollipop plot in ggplot](https://stackoverflow.com/questions/50135338/text-labels-on-condition-for-lollipop-plot-in-ggplot)
  ## Reference [Custom lollipop chart](https://r-graph-gallery.com/301-custom-lollipop-chart.html)
  output$facet.lollipop.plot.monthly.spend.on.food.categories <- shiny::renderPlot({
    ggplot(data = expenditure.food.category
           ,aes(x=price.summed
                ,y=reorder(timestamp.month.label, lubridate::my(timestamp.month.label))
                ,fill = category.firstup
                ,label=round(price.summed, digits = 0)))+
      geom_segment(aes(y=reorder(timestamp.month.label, lubridate::my(timestamp.month.label))
                       ,yend=reorder(timestamp.month.label, lubridate::my(timestamp.month.label))
                       ,x=0
                       ,xend = price.summed)
                   ,color="skyblue")+
      geom_point(size=7.5, color="blue", alpha=0.6)+
      geom_text(size =3, color="white" #nudge_x = 0.1
      ) +
      facet_wrap(~category.firstup, ncol = 3, scales = "free_x",strip.position = "top")+
      ggplot2::labs( y="Month"
                    ,x="Spend ($AUD)")+
      theme_minimal()+
      # Apply The Economist theme
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 10)
        ,legend.position = "none"
        ,panel.grid.major.y = element_blank()
        ) # Close theme()
    })

  #---------------
  # Output infoBox 
  ##  Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #---------------
  function.renderInfoBox(output.id="infoBox.food.barcode.earliest.date.record"
                         ,arg.title="Data collected since"
                         ,arg.value=food.barcode.earliest.date.record)
  
  function.renderInfoBox(output.id="infoBox.food.barcode.latest.date.record"
                         ,arg.title="Latest update on"
                         ,arg.value=food.barcode.latest.date.record)
  
  #*****************************************
  # Outputs to use under menuItem "Bathroom"
  #*****************************************
  
  #----------------
  # Output valueBox
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #----------------
  output$valueBox.num.unique.hygiene.barcode <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value= num.unique.hygiene.barcode
      ,subtitle = "Personal Care Product Barcodes scanned"
      ,icon = icon("th-list",lib = "glyphicon")
      ,color = color.global.valueBox #"purple"
      )
  })
  
  output$valueBox.summed.price.hygiene <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value= paste0("$AUD ", summed.price.hygiene)
      ,subtitle = "Spent on personal care products"
      ,icon = icon("credit-card", lib = "glyphicon")
      ,color = color.global.valueBox #"green"
      )
  })
  
  #----------------------------------------------------------------------------------------
  # Output DT dataTable
  ## Ref [How can I introduce a new line within a column using DTedit and shiny::uiOutput?](https://stackoverflow.com/questions/56590555/how-can-i-introduce-a-new-line-within-a-column-using-dtedit-and-shinyuioutput)
  #----------------------------------------------------------------------------------------
  
  output$table.hygiene.count.unopened.DT <- DT::renderDataTable({
    #print(hygiene.count.unopened)
    DT::datatable(hygiene.count.unopened, options = list(autoWidth=TRUE, searching=TRUE))
  }) # Close the renderDataTable function
  
  output$table.hygiene.price.DT <- DT::renderDataTable({
    #print(hygiene.price)
    DT::datatable(hygiene.price, escape=FALSE, options = list(autoWidth=TRUE, searching=TRUE))
  }
  #, escape=FALSE
  ) # Close the renderDataTable function

  #**********************************************
  # Outputs to use under menuItem "Recycling"
  #**********************************************
  
  #----------------
  # Output valueBox
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  ## [How to break line inside a paste0() function in shiny dashboard](https://stackoverflow.com/questions/51079153/how-to-break-line-inside-a-paste0-function-in-shiny-dashboard)
  #----------------
  output$valueBox.numb.all.containers.stock <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = numb.all.containers.stock
      ,subtitle = HTML(paste0("Containers in stock, including",br()
                              ,"<b>",numb.PET.stock," plastic bottles","</b>", br()
                              ,"<b>",numb.cans.stock," cans","</b>", br()
                              ,"<b>",numb.glass.stock," glass bottles","</b>", br()
                              ,"<b>",numb.carton.stock," carton","</b>")
                       ) # Close HTML()
      ,color = color.global.valueBox )
  })
  output$valueBox.numb.collections.made <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value= numb.collections.made
      ,subtitle = "Collections made"
      ,color = color.global.valueBox)
  })
  output$valueBox.numb.containers.collected <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = numb.all.containers.collected
      ,subtitle = HTML(paste0("Containers collected, including",br()
                              ,"<b>", numb.PET.collected," plastic bottles","</b>", br()
                              ,"<b>", numb.cans.collected," cans","</b>", br()
                              ,"<b>", numb.glass.collected, " glass bottles","</b>", br()
                              ,"<b>", numb.carton.collected, " carton","</b>")
                       ) # Close HTML()
      ,color = color.global.valueBox ) # Close valueBox()
  }) # Close renderValueBox()
  output$valueBox.numb.refunds.received <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value= numb.refunds.received
      ,subtitle = "Refunds received"
      ,color = color.global.valueBox )
  })
  output$valueBox.numb.containers.refunded <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value= numb.all.containers.refunded
      ,subtitle = HTML(paste0("Containers refunded, including", br()
                              ,"<b>", numb.PET.refunded," plastic bottles","</b>", br()
                              ,"<b>", numb.cans.refunded," cans","</b>", br()
                              ,"<b>", numb.glass.refunded, " glass bottles","</b>", br()
                              ,"<b>", numb.carton.refunded, " carton","</b>")
                       ) # Close HTML()
      ,color = color.global.valueBox ) # Close valueBox()
  })
  
  #-----------------------
  # Output plots
  #-----------------------
  ## Rendering, in web development, is the process of converting code into viewable, interactive web content.
  output$plot.stacked.bars.containers <- shiny::renderPlot({
    #--------------------------------------------------
    # Create upside down and bottom up stacked bar plot
    #--------------------------------------------------
    
    # bar width, position setttings
    width <- .75
    position <- ggplot2::position_stack(vjust=.5)
    
    # Display color from color code
    ## #8 light blue for PET, #2 red for cans, #9 brown for glass, #11 purple for carton
    colors <- as.vector(pals::glasbey(n=11))[c(8,2,9,11)]
    #scales::show_col(colors)
    
    # Pair legend item and color
    legend.items.color <- c( PET=colors[1], cans=colors[2], glass=colors[3],carton=colors[4])
    plot.legend.label.ordered <- c("Plastic bottles","Cans","Glass bottles","Carton") # length(plot.legend.ordered) 4
    
    # Create a stacked bar plot with total number placed on top over each bar
    ggplot2::ggplot(data=containers.daily.long.not.all.types
                    ,aes(x=date.of.activity
                         ,y=container.number.adjusted
                         ,label=container.number.adjusted)) +
      # geom_col() as a shortcut for geom_bar(stat = "identity")
      ggplot2::geom_col(
        # Stacks of the bar order, from top to bottom, controlled by the levels
        aes(fill=factor(container.type, levels = c("PET","cans","glass","carton")))
        # Control transparency of bar color
        ,alpha=0.6)+
      # Draw sum number above the stacked bars using another dataset totals.all.types
      ggplot2::geom_text(data=totals.all.types
                         ,aes(x=date.of.activity, y=total, label=total.label, fill=NULL)
                         ,vjust=0.5
                         ,size=5
                         ,angle=90
                         ,hjust=0.4)+
      # Months on bottom X axis, weeks on top X axis
      ggplot2::scale_x_date(expand = c(0, 0) # expand = c(0,0) to remove margins
                            ,date_breaks = "1 month"
                            ,date_labels = "%b-%Y" # %b Abbreviated month name in current locale (Aug)
                            ,date_minor_breaks = "1 week"
                            ) +
      # Set breaks on Y axis
      ggplot2::scale_y_continuous(breaks = seq(0, 300, 50))+
      ggplot2::labs(x = ""
                    ,y = "Number of containers"
                    # Change legend title from variable name to text
                    ,fill="Container type ")+
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        legend.position = "top"
        ,legend.justification='left'
        ,legend.direction='horizontal'
        ,legend.text = element_text(size = 20)
        ,legend.box = "vertical"
        # Reduce gap between legends
        ,legend.spacing.y = unit(-0.25, "cm")
        ,axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 15)
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(color = "gray", linewidth = 0.5))+
      # Modify legend for aesthetic=fill
      ggplot2::scale_fill_manual(
        # Change legend item color from default to colors
        # Use palette from pals package. as.vector() needed to remove color name
        values=legend.items.color
        ,labels=plot.legend.label.ordered)
  }) # Close renderPlot()
  
  #--------------------
  # Output DT dataTable
  #--------------------
  output$table.containers.daily.DT <- DT::renderDataTable({
    containers.daily.wide |> 
      dplyr::select(date.of.activity, activities, number.PET.day, number.cans.day, number.glass.day, number.carton, number.all.types.day, note) |>
      dplyr::rename( Date=date.of.activity
                    ,Activity=activities
                    ,Plastic=number.PET.day
                    ,Cans=number.cans.day
                    ,Glass=number.glass.day
                    ,Carton=number.carton
                    ,Total=number.all.types.day
                    ,Note=note)
    
  }) # Close the renderDataTable function

  #---------------
  # Output infoBox 
  ##  Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #---------------
  output$infoBox.date.earliest.record.recycling <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = "Data collected since"
      ,value = date.earliest.record.recycling
      ,icon = icon("list")
      ,color = color.global.infoBox #"olive"
      ,fill= TRUE)
  })
  
  output$infoBox.date.latest.record.recycling <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = "Latest update on"
      ,value = date.latest.record.recycling
      ,icon = icon("list")
      ,color = color.global.infoBox #"light-blue"
      ,fill= TRUE)
  })
  
  #***************************************************
  # Data to use by outputs under menuItem "Fitness"
  #***************************************************
  
  #-----------------------------------------------------------------
  # Read activities.csv from Google Drive if no file input from user
  #-----------------------------------------------------------------
  get_file_or_default <- shiny::reactive({
    # Read activities.csv from Google Drive if no file input from user
    ## Set Google sheet to Share > Anyone with the link > copy URL > get Google file ID 
    ## The File ID can be found in the URL of the file when it is opened on Google Drive. It is the combination of letters and numbers that appear after "d/" in the link: https://docs.google.com/spreadsheets/d/***ThisIsFileID***/edit#gid=123456789
    ## https://drive.google.com/file/d/1LGjKcVNWoU1H-khns3ZaS9jUMMHIbcXd/view?usp=sharing
    if (is.null(input$file1)){
      Google.file.ID <- "1LGjKcVNWoU1H-khns3ZaS9jUMMHIbcXd" #"1MndqdRoXOd1vegazd-CJyPZKl41SHpYh"
      df <- utils::read.csv(file= sprintf("https://docs.google.com/uc?id=%s&export=download", Google.file.ID)
                            ,header = TRUE) # dim(df) 704 87
      } else{
      # Read CSV file via user upload
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      shiny::req(file)
      shiny::validate(need(ext == "csv", message = "Please upload activities.csv downloaded from Strava"))
      
      # input$file1 directly, but this is a variable that represents the return of a fileInput, which is a data.frame, the data.frame contains a column datapath, that is the actual path to the copy of the file as it is available to your app. so you should be looking to use input$file1$datapath for that role     
      # input$file1 will be NULL initially. After the user selects and uploads a file, head of that data file by default, or all rows if selected, will be shown.
      
      df <- utils::read.csv(file$datapath, header = TRUE)
    } # Close else
    
    # Return data value
    df
  }) # Close reactive
  
  #-----------------------------------
  # Process data.frame from reactive()
  #-----------------------------------
  activities.df <- shiny::reactive({
    
    format.datetime <- "%b %d, %Y, %I:%M:%S %p"
    
    activities.df <- get_file_or_default() |> 
      dplyr::select(Activity.ID, Activity.Date, Activity.Name, Activity.Gear, Activity.Type, Distance, Elevation.Gain, Elapsed.Time, Moving.Time, Max.Heart.Rate) |>
      dplyr::mutate(
        start.datetime.UTC=as.POSIXct(x=Activity.Date, format = format.datetime, tz="UTC")
        ,start.date.UTC=lubridate::date(start.datetime.UTC)
        ,start.datetime.local=case_when(
          start.date.UTC >= ymd('2023-01-16') & start.date.UTC <= ymd('2023-02-05')~
            as.POSIXct(start.datetime.UTC, tz="Asia/Kuala_Lumpur")
          ,start.date.UTC >= ymd('2023-09-02') & start.date.UTC <= ymd('2023-10-02') ~ as.POSIXct(start.datetime.UTC, tz="Asia/Taipei")
          ,TRUE ~ as.POSIXct(start.datetime.UTC, tz="Australia/Brisbane")
        )
        ,start.date.local=lubridate::date(start.datetime.local)
        ,start.year.local=lubridate::year(start.datetime.local)
        ,start.dayofyear.local=lubridate::yday(start.datetime.local)
        ,start.month.local=lubridate::month(start.datetime.local)
        ,start.day.local=weekdays(start.datetime.local)
        ,start.week.local=lubridate::week(start.datetime.local)
        # Distance contains decimal places for cycling running distance, and comma for swim distance
        ,distance.km= dplyr::case_when(
          Activity.Type=="Swim" ~ as.numeric(gsub(pattern=",", replacement=".", x=Distance))
          ,Activity.Type %in% c("Ride","Run","E-Bike Ride", "Walk") ~ as.numeric(Distance)
          ,TRUE ~ NA_integer_)
        ,elevation.gain.m= Elevation.Gain # Elevation gain in meters
        ,elapsed.time.hour=Elapsed.Time/60/60
        ,moving.time.hour=Moving.Time/60/60) |>
      dplyr::select(-Distance, -Elevation.Gain, -Elapsed.Time, -Moving.Time) # dim(activities.df) 704 19
    
    # Return data value
    activities.df
  }) # Close reactive()
  
  #--------------------
  # Output data.frame 2
  #--------------------
  activities.2023 <- shiny::reactive({
    # Subset data from the year to analyse
    activities.2023 <- activities.df() |> 
      dplyr::filter(start.year.local==2023 & Activity.Type %in% c("Ride","Run","Swim","Workout","Walk")) |>
      # Split Activity.Type="Workout"
      dplyr::mutate(activity.type=dplyr::case_when(
        grepl(pattern="table tennis", x=Activity.Name, ignore.case=TRUE) ~ 
          stringi::stri_trans_totitle("table tennis")
        ,grepl(pattern="badminton", x=Activity.Name, ignore.case=TRUE) ~ 
          stringi::stri_trans_totitle("badminton")
        ,grepl(pattern="rehabilitation exercise|strength and stability exercises|dry land exercises", x=Activity.Name, ignore.case=TRUE) ~ 
          stringi::stri_trans_totitle("strength & stability workout")
        ,grepl(pattern="bike fitting", x=Activity.Name, ignore.case=TRUE) ~ 
          stringi::stri_trans_totitle("bike fitting")
        ,TRUE ~ Activity.Type )) # dim(activities.2023) 385 20
    
    # return data value
    activities.2023
  }) # Close reactive()
  
  #---------------------------------------------
  # Yearly longest ride, greatest elevation ride
  #---------------------------------------------
  year.ride.max.distance.elevation <- shiny::reactive({
    # Collapse cycling data (Activity type= Ride) to 1 record per day
    year.ride.max.distance <- activities.df() |> 
      # If you don't set a default gear in Strava, it is a missing value in activities.csv
      dplyr::filter(Activity.Type=="Ride") |>
      # Daily sum of distance and elevation gain
      dplyr::group_by(start.year.local) |>
      dplyr::filter(distance.km == max(distance.km, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::mutate(Highlight= paste("Longest ride in", start.year.local)) |>
      dplyr::select(start.date.local, Activity.Name, distance.km, elevation.gain.m, Highlight) # dim(year.ride.max.distance) 4 5
    
    year.ride.max.elevation <- activities.df() |> 
      # If you don't set a default gear in Strava, it is a missing value in activities.csv
      dplyr::filter(Activity.Type=="Ride") |>
      # Daily sum of distance and elevation gain
      dplyr::group_by(start.year.local) |>
      dplyr::filter(elevation.gain.m == max(elevation.gain.m, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::mutate(Highlight= paste("Greatest elevation gain in", start.year.local)) |>
      dplyr::select(start.date.local, Activity.Name, distance.km, elevation.gain.m, Highlight) # dim(year.ride.max.elevation) 4 5
    
    # Stack year.ride.max.distance and year.ride.max.elevation  
    year.ride.max.distance.elevation <- rbind(year.ride.max.distance, year.ride.max.elevation) |>
      dplyr::arrange(start.date.local) |> 
      dplyr::mutate(elevation.gain.m=format(round(elevation.gain.m, digits = 0), nsmall = 0)) |>
      dplyr::rename(Date=start.date.local
                    ,Activity=Activity.Name
                    ,`Distance (km)`= distance.km
                    ,`Elevation gain (m)`=elevation.gain.m) # dim(year.ride.max.distance.elevation) 8 5
    # Return data value
    year.ride.max.distance.elevation
  })# Close reactive()
  
  #--------------------
  # Output data.frame 3
  #--------------------
  bike.ride.day <- shiny::reactive({
    # Collapse cycling data (Activity type= Ride) to 1 record per day
    bike.ride.day <- activities.df() |> 
      # If you don't set a default gear in Strava, it is a missing value in activities.csv
      dplyr::filter(Activity.Type=="Ride" & Activity.Gear=="Merida_Ride400") |>
      # Daily sum of distance and elevation gain
      dplyr::group_by(start.year.local, start.date.local, start.day.local, start.week.local, start.dayofyear.local) |>  
      dplyr::summarise(
        activity.name.day=paste0(Activity.Name, collapse = ",")
        #activity.name.day=paste(Activity.Name, collapse = "<br>")  
        ,num.ride.day=dplyr::n()
        ,distance.km.day= sum(distance.km, na.rm=TRUE)
        ,elevation.gain.m.day= sum(elevation.gain.m, na.rm=TRUE)) |> 
      # Accumulate distance, elevation gain yearly
      dplyr::group_by(start.year.local) |>
      dplyr::arrange(start.date.local) |>
      dplyr::mutate(ride.distance.cum.year = cumsum(distance.km.day)
                    ,ride.elevation.cum.year = cumsum(elevation.gain.m.day)) # dim(bike.ride.day) 204 11
    # Return data value
    bike.ride.day
  })# Close reactive()
  
  #--------------------
  # Output data.frame 4
  #--------------------
  bike.ride.count.distance.elevation.total <- shiny::reactive({
    # Collapse cycling data to 1 record per day of week and year
    ## Two year data will have 7*2 records
    bike.ride.count.distance.elevation.total <- activities.df() |> 
      # If you don't set a default gear in Strava, it is a missing value in activities.csv
      dplyr::filter(Activity.Type=="Ride" & Activity.Gear=="Merida_Ride400") |>
      dplyr::group_by(start.year.local, start.day.local) |>
      dplyr::summarise(count.ride=dplyr::n()
                       ,total.ride.distance.dayOfWeek=sum(distance.km)
                       ,total.ride.elevation.dayOfWeek=sum(elevation.gain.m)
      ) # dim(bike.ride.count.distance.elevation.total) 14 5
    # Return data value
    bike.ride.count.distance.elevation.total
  })# Close reactive()
  
  #--------------------
  # Output data.frame 5
  #--------------------
  bike.ride.max.count.dayOfWeek <- shiny::reactive({
    # Collapse cycling summary data to 1 record per year
    bike.ride.max.count.dayOfWeek <- bike.ride.count.distance.elevation.total() |>
      dplyr::group_by(start.year.local) |>
      dplyr::filter(count.ride==max(count.ride)) |>
      dplyr::select(-total.ride.distance.dayOfWeek,-total.ride.elevation.dayOfWeek) # dim(bike.ride.max.count.dayOfWeek) 2 3
    # Return data value
    bike.ride.max.count.dayOfWeek
  })
  
  #--------------------
  # Output data.frame 6
  #--------------------
  bike.ride.longest.distance.dayOfWeek <- shiny::reactive({
    # Collapse cycling summary data to 1 record per year
    bike.ride.longest.distance.dayOfWeek <- bike.ride.count.distance.elevation.total() |>
      dplyr::group_by(start.year.local) |>
      dplyr::filter(total.ride.distance.dayOfWeek==max(total.ride.distance.dayOfWeek)) |>
      dplyr::select(-count.ride, -total.ride.elevation.dayOfWeek) # dim(bike.ride.longest.distance.dayOfWeek) 2 3
    # Return data value
    bike.ride.longest.distance.dayOfWeek
  })
  
  #--------------------
  # Output data.frame 7
  #--------------------
  bike.ride.highest.elevation.dayOfWeek <- shiny::reactive({
    # Collapse cycling summary data to 1 record per year
    bike.ride.highest.elevation.dayOfWeek <- bike.ride.count.distance.elevation.total() |>
      dplyr::group_by(start.year.local) |>
      dplyr::filter(total.ride.elevation.dayOfWeek==max(total.ride.elevation.dayOfWeek)) |>
      dplyr::select(-count.ride, -total.ride.distance.dayOfWeek) # dim(bike.ride.highest.elevation.dayOfWeek) 2 3
    # Return data value
    bike.ride.highest.elevation.dayOfWeek
  })
  
  #*****************************************
  # Outputs to use under menuItem "Fitness"
  #*****************************************
  
  #---------------------
  # Output markdown text
  #---------------------
  # output$markdown.menuItem.fitness <- shiny::renderUI({
  #   rmarkdown::render(input = "menuItem-Fitness.Rmd"
  #                     ,output_format = html_document(self_contained = TRUE)
  #                     ,output_file = "menuItem-Fitness.html")
  #   shiny::includeHTML('menuItem-Fitness.html')
  # }) # Close renderUI()
  
  # Read html instead of Rmd
  #output$inc <- renderUI(includeHTML("./menuItem-Fitness.html"))
  #-------------------------
  # Output infoBox, valueBox
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #-------------------------
  output$valueBox.years.in.sport <- shinydashboard::renderValueBox({
    years.in.sport <- unique(activities.2023()$start.year.local)
    shinydashboard::valueBox(
      value = years.in.sport
      ,subtitle = "Year in sport"
      ,icon = icon("list")
      ,color = color.global.valueBox #"purple"
      )
  })
  
  output$valueBox.number.days.active <- shinydashboard::renderValueBox({
    number.days.active <- length(unique(activities.2023()$start.date.local))
    shinydashboard::valueBox(
      value = number.days.active
      ,subtitle = "Days active"
      ,icon = icon("list")
      ,color = color.global.valueBox #"yellow"
      )
  })
  
  output$valueBox.total.moving.time <- shinydashboard::renderValueBox({
    total.moving.time <- round(sum(activities.2023()$moving.time.hour, na.rm = TRUE), digits=0)
    shinydashboard::valueBox(
       value = paste(total.moving.time, "hours")
      ,subtitle = "Total moving time"
      ,icon = icon("list")
      ,color = color.global.valueBox #"yellow"
      )
  })
  
  output$valueBox.total.cycling.distance <- shinydashboard::renderValueBox({
    total.cycling.distance <- format(round(sum(activities.2023()$distance.km, na.rm = TRUE), digits=0), nsmall=0, big.mark=",")
    shinydashboard::valueBox(
      value = paste(total.cycling.distance, "km")
      ,subtitle = "Total cycling distance"
      ,icon = icon("list")
      ,color = color.global.valueBox #"orange"
      )
  })
  
  output$valueBox.total.cycling.elevation <- shinydashboard::renderValueBox({
    total.cycling.elevation <- format(round(sum(activities.2023()$elevation.gain.m, na.rm = TRUE), digits=0), nsmall=0, big.mark=",")
    shinydashboard::valueBox(
      value = paste(total.cycling.elevation, "m")
      ,subtitle = "Total cycling elevation gain"
      ,icon = icon("list")
      ,color = color.global.valueBox #"orange"
      )
  })
  #--------------
  # Output plots
  #--------------
  
  #-----------------------------------------------------------------------------------------
  # Create a bar plot of daily moving time in hour
  ## Ref [how to correctly use guide_legend to control the number of rows of legend](https://stackoverflow.com/questions/35465836/how-to-correctly-use-guide-legend-to-control-the-number-of-rows-of-legend)
  #-----------------------------------------------------------------------------------------
  output$plot.barplot.activity.moving.time <- shiny::renderPlot({
    # Read the data.frame created by the reactive()
    data.barplot.moving.time <- activities.2023() # dim(data.barplot.moving.time) 385 20
    
    # Legend
    plot.legend.ordered.barplot.moving.time <- sort(unique(data.barplot.moving.time$activity.type)) # length(plot.legend.ordered.barplot.moving.time) 8
    
    # Color. Use a palette with multiple distinct colors
    colors.barplot.moving.time <- as.vector(pals::glasbey(n=length(plot.legend.ordered.barplot.moving.time)))
    
    # Pair legend item and color
    legend.colors.barplot.moving.time <- c( Badminton=colors.barplot.moving.time[1]
                                            ,`Bike Fitting`=colors.barplot.moving.time[2]
                                            ,Ride=colors.barplot.moving.time[3]
                                            ,Run=colors.barplot.moving.time[4]
                                            ,`Strength & Stability workout`=colors.barplot.moving.time[5]
                                            ,Swim=colors.barplot.moving.time[6]
                                            ,`Table Tennis`=colors.barplot.moving.time[7]
                                            ,Walk=colors.barplot.moving.time[8])
    # Create a bar plot
    barplot.daily.moving.time <- ggplot2::ggplot(data.barplot.moving.time
                                                 , aes(x=start.date.local, y=moving.time.hour, fill = activity.type)
    )+
      ggplot2::geom_bar(position = "stack", stat = "identity") + 
      ggplot2::scale_x_date( 
        limits = as.Date(c('2023-01-01','2023-12-31'))
        ,expand = c(0, 0) # expand = c(0,0) to remove margins
        ,date_breaks = "1 month"
        ,date_minor_breaks = "1 week"
        ,date_labels = "%b" # Abbreviated month name in current locale (Aug)
        # Add a secondary x axis showing week number
        # ,sec.axis = ggplot2::sec_axis(
        #   trans= ~ .
        #   ,breaks= c(seq.Date(as.Date("2023-01-01"), by="month", length.out = 12)
        #              ,"2023-12-25")
        #   ,labels= scales::date_format("%W"))
        )+
      ggplot2::scale_y_continuous(limits = c(0,8)
                                  ,breaks = seq(from=0, to=8, by=1)
                                  ,expand = c(0, 0) # expand = c(0,0) to remove margins
      )+
      ggplot2::labs(x = ""
                    ,y = "Moving time (h)"
                    ,fill="Activity type")+
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+ # theme color gray
      #ggthemes::theme_economist()+ # theme color pale green
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        legend.position = "top"
        ,legend.justification='left'
        ,legend.direction='horizontal'
        ,legend.text = element_text(size = 15)
        ,legend.box = "vertical"
        # Reduce gap between legends
        ,legend.spacing.y = unit(-0.25, "cm")
        ,axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 15)
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(color = "gray", linewidth = 0.5))+
      # Change bar colors
      ## Use ggplot2::scale_color_manual() to modify aesthetics = "colour"
      ## Use ggplot2::scale_fill_manual() to modify aesthetics = "fill"
      ggplot2::scale_fill_manual(
        # Use palette from pals package. as.vector() needed to remove color name
        values=legend.colors.barplot.moving.time
        # Reorder legend items from most frequent to least frequent
        # legend text longer than the cutoff is wrapped to multiple lines
        ,limits=stringr::str_wrap(plot.legend.ordered.barplot.moving.time,width=20))+ 
      # Control legend for fill= aesthetic
      ## This has no effect. Legend items are still in two lines 
      ggplot2::guides(colour = guide_legend(nrow = 1))
    
    # Return value as default display
    return(barplot.daily.moving.time)
  }) # Close renderPlot()
  
  #---------------------------------
  # Output plot data as DT dataTable
  ## plot=barplot.daily.moving.time
  #---------------------------------
  output$table.barplot.daily.moving.time.DT <- DT::renderDataTable({
    activities.2023() |> 
      dplyr::select(start.date.local, Activity.Name, activity.type, moving.time.hour, Max.Heart.Rate, Activity.Gear) |>
      dplyr::mutate(moving.time.hour=round(moving.time.hour, digits = 2)) |>
      dplyr::rename( Date=start.date.local
                     ,Activity=Activity.Name
                     ,Type=activity.type
                     ,`Moving time`=moving.time.hour
                     ,`Max heart rate`=Max.Heart.Rate
                     ,Gear=Activity.Gear)
  }) # Close the renderDataTable function
  
  #-------------------------------------------------------------------------------------------------------------
  # Plot cumulative cycling distance over start.date.local
  ## Strava post content: Comparing cumulative riding distances between 2023 and 2022
  ## Ref [Plot line and bar graph (with secondary axis for line graph) using ggplot](https://stackoverflow.com/questions/53922846/plot-line-and-bar-graph-with-secondary-axis-for-line-graph-using-ggplot)
  ## Ref [ggplot with 2 y axes on each side and different scales](https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales)
  ## Ref [Analyzing Strava data using R](https://www.andrewaage.com/post/analyzing-strava-data-using-r/)
  ## Ref [Error when using gghighlight to highlight one group of data](https://stackoverflow.com/questions/75883553/error-when-using-gghighlight-to-highlight-one-group-of-data)
  ## Ref [10 tips for making your R graphics look their best](https://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html)
  #-------------------------------------------------------------------------------------------------------------
  output$plot.lineplot.ride.distance <- shiny::renderPlot({
    # Take data.frame outputted by reactive()
    data.lineplot.ride.distance <- bike.ride.day() # dim(data.lineplot.ride.distance) 204 11
    
    # Plot cumulative riding distance per year
    lineplot.ride.distance <- ggplot2::ggplot(data=data.lineplot.ride.distance
                                              ,aes(x=start.dayofyear.local
                                                   ,y=ride.distance.cum.year
                                                   ,color=factor(start.year.local)))+
      ggplot2::geom_line()+
      ggplot2::scale_color_brewer(palette = "Set1")+
      # Formatting y axis label with commas 
      ggplot2::scale_y_continuous(labels = scales::comma)+
      gghighlight::gghighlight(start.year.local >= 2022, use_group_by = FALSE) +
      ggplot2::labs( x="Day of year"
                    ,y=""
                    ,color="Year")+
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+ # theme color gray
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        legend.position = "top"
        ,legend.justification='left'
        ,legend.direction='horizontal'
        ,legend.text = element_text(size = 15)
        ,legend.box = "vertical"
        # Reduce gap between legends
        ,legend.spacing.y = unit(-0.25, "cm")
        ,axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 15)
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(color = "gray", linewidth = 0.5))
    
    # Return value as default display
    return(lineplot.ride.distance)
  })
  
  #--------------------------------------------------------
  # Plot cumulative cycling elevation over start.date.local
  #--------------------------------------------------------
  output$plot.lineplot.ride.elevation <- shiny::renderPlot({
    # Take data.frame outputted by reactive()
    data.lineplot.ride.elevation <- bike.ride.day() # dim(data.lineplot.ride.elevation) 204 11
    
    # Create a line plot
    lineplot.ride.elevation <- ggplot2::ggplot(data=data.lineplot.ride.elevation
                                               ,aes(x=start.dayofyear.local
                                                    ,y=ride.elevation.cum.year
                                                    ,color=factor(start.year.local)))+
      ggplot2::geom_line()+
      ggplot2::scale_color_brewer(palette = "Set1")+
      # Formatting y axis label with commas 
      ggplot2::scale_y_continuous(labels = scales::comma)+
      gghighlight::gghighlight(start.year.local >= 2022, use_group_by = FALSE) +
      ggplot2::labs(x="Day of year"
                    ,y=""
                    ,color="Year") +
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+ # theme color gray
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        legend.position = "top"
        ,legend.justification='left'
        ,legend.direction='horizontal'
        ,legend.text = element_text(size = 15)
        ,legend.box = "vertical"
        # Reduce gap between legends
        ,legend.spacing.y = unit(-0.25, "cm")
        ,axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 15)
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(color = "gray", linewidth = 0.5))
    
    # Return value as default display
    return(lineplot.ride.elevation)
  })
  
  #---------------------------------
  # Output plot data as DT dataTable
  ## plot=lineplot.ride.elevation
  #---------------------------------
  output$table.lineplot.ride.elevation.DT <- DT::renderDataTable({
    bike.ride.day() |> 
      dplyr::ungroup()|>
      dplyr::mutate(start.date.local.start.day.local= paste(start.date.local, start.day.local)
                    ,elevation.gain.m.day=round(elevation.gain.m.day, digits = 0)
                    ,ride.elevation.cum.year=round(ride.elevation.cum.year, digits = 0)) |>
      dplyr::select(start.date.local.start.day.local, start.dayofyear.local, activity.name.day, elevation.gain.m.day, ride.elevation.cum.year) |>
      dplyr::rename( Date=start.date.local.start.day.local
                     ,`Day of year`=start.dayofyear.local
                     ,Activities=activity.name.day
                     ,`Elevation gain`=elevation.gain.m.day
                     ,`Running total`=ride.elevation.cum.year)
  }) # Close the renderDataTable function
  
  #-------------------------------------
  # Output infoBox
  ## plot=calendar.heatmap.ride.distance
  ## plot=calendar.heatmap.ride.elevation
  #-------------------------------------
  #-----
  # 2022
  #-----
  output$infoBox.weekday.greatest.number.rides.2022 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = paste("Most active day of week in", bike.ride.max.count.dayOfWeek()$start.year.local[1])
      ,value = paste(bike.ride.max.count.dayOfWeek()$start.day.local[1]
                     ,"("
                     ,bike.ride.max.count.dayOfWeek()$count.ride[1]
                     ,"rides)")
      ,icon = icon("list")
      ,fill = TRUE
      ,color = color.global.valueBox
      )
  }) # Close renderInfoBox()
  output$infoBox.weekday.longest.ride.distance.2022 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = paste("Most productive in distance in", bike.ride.max.count.dayOfWeek()$start.year.local[1])
      ,value = paste(bike.ride.longest.distance.dayOfWeek()$start.day.local[1]
                     ,"( total"
                     ,format(bike.ride.longest.distance.dayOfWeek()$total.ride.distance.dayOfWeek[1]
                             ,nsmall=0
                             ,big.mark=",")
                     ,"kilometers)")
      ,icon = icon("list")
      ,fill = TRUE
      ,color = color.global.valueBox
      )
  }) # Close renderInfoBox()
  output$infoBox.weekday.greatest.ride.elevation.2022 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = paste("Most productive in elevation gain in", bike.ride.max.count.dayOfWeek()$start.year.local[1])
      ,value = paste(bike.ride.longest.distance.dayOfWeek()$start.day.local[1]
                     ,"( total"
                     ,format(round(bike.ride.highest.elevation.dayOfWeek()$total.ride.elevation.dayOfWeek[1],digits = 2)
                             ,nsmall = 2
                             ,big.mark = ",")
                     ,"meters)")
      ,icon = icon("list")
      ,fill = TRUE
      ,color = color.global.valueBox)
  }) # Close renderInfoBox()
  
  #-----
  # 2023
  #-----
  output$infoBox.weekday.greatest.number.rides.2023 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = paste("Most active day of week in", bike.ride.max.count.dayOfWeek()$start.year.local[2])
      ,value = paste(bike.ride.max.count.dayOfWeek()$start.day.local[2]
                     ,"("
                     ,bike.ride.max.count.dayOfWeek()$count.ride[2]
                     ,"rides)")
      ,icon = icon("list")
      ,fill = TRUE
      ,color = color.global.valueBox
    )
  }) # Close renderInfoBox()
  
  output$infoBox.weekday.longest.ride.distance.2023 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = paste("Most productive in distance in", bike.ride.max.count.dayOfWeek()$start.year.local[2])
      ,value = paste(bike.ride.longest.distance.dayOfWeek()$start.day.local[2]
                     ,"( total"
                     ,format(bike.ride.longest.distance.dayOfWeek()$total.ride.distance.dayOfWeek[2]
                             ,nsmall=0
                             ,big.mark=",")
                     ,"kilometers)")
      ,icon = icon("list")
      ,fill = TRUE
      ,color = color.global.valueBox
    )
  }) # Close renderInfoBox()
  
  output$infoBox.weekday.greatest.ride.elevation.2023 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = paste("Most productive in elevation gain in", bike.ride.max.count.dayOfWeek()$start.year.local[2])
      ,value = paste(bike.ride.longest.distance.dayOfWeek()$start.day.local[1]
                     ,"( total"
                     ,format(round(bike.ride.highest.elevation.dayOfWeek()$total.ride.elevation.dayOfWeek[2],digits = 2)
                             ,nsmall = 2
                             ,big.mark = ",")
                     ,"meters)")
      ,icon = icon("list")
      ,fill = TRUE
      ,color = color.global.valueBox)
  }) # Close renderInfoBox()
  
  #-------------------------------------------------------------------------------------------------------------
  # Plot cycling distance per day with calendar heatmap
  ## X: week of year, Y axis: weekday, value: cycling distance
  #-------------------------------------------------------------------------------------------------------------
  output$plot.calendar.heatmap.ride.distance <- shiny::renderPlot({
    # Take data.frame outputted by reactive()
    data.calendar.heatmap.ride.distance <- bike.ride.day() # dim(data.calendar.heatmap.ride.distance) 204 11

    # Create a github-style calender heat-map using ggplot
    calendar.heatmap.ride.distance <- ggplot2::ggplot(
      data=data.calendar.heatmap.ride.distance
      ,aes( x=start.week.local
            ,y=factor(data.calendar.heatmap.ride.distance$start.day.local
                      ,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))))+
      ggplot2::geom_tile(aes(fill=distance.km.day))+
      ggplot2::scale_fill_continuous(low = "lightgreen", high = "red") +
      ggplot2::facet_wrap(~ start.year.local, scales = "free_x") +
      ggplot2::labs(  x = "Week"
                     ,y = ""
                     ,fill="Distance per day")+
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+ # theme color gray
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 15)
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(color = "gray", linewidth = 0.5))
    
    # Return value
    return(calendar.heatmap.ride.distance)
  })
  #------------------------------------------------------------
  # Plot cycling elevation per day with calendar heatmap
  ## X: week of year, Y axis: weekday, value: cycling elevation
  #------------------------------------------------------------
  output$plot.calendar.heatmap.ride.elevation <- shiny::renderPlot({
    # Take data.frame outputted by reactive()
    data.calendar.heatmap.ride.elevation <- bike.ride.day() # dim(data.calendar.heatmap.ride.elevation) 204 11
    bike.ride.max.count.dayOfWeek <- bike.ride.max.count.dayOfWeek() # dim(bike.ride.max.count.dayOfWeek) 2 3
    bike.ride.highest.elevation.dayOfWeek <- bike.ride.highest.elevation.dayOfWeek() # dim(bike.ride.highest.elevation.dayOfWeek) 2 3
    
    # Create a github-style calender heat-map using ggplot
    calendar.heatmap.ride.elevation <- ggplot2::ggplot(
      data=data.calendar.heatmap.ride.elevation
      ,aes( x=start.week.local
            ,y=factor(data.calendar.heatmap.ride.distance$start.day.local
                      ,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))))+
      ggplot2::geom_tile(aes(fill=elevation.gain.m.day))+
      ggplot2::scale_fill_continuous(low = "lightgreen", high = "red") +
      ggplot2::facet_wrap(~ start.year.local, scales = "free_x") +
      ggplot2::labs(  x = "Week"
                     ,y = ""
                     ,fill="Elevation per day")+
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+ # theme color gray
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
         axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 15)
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(color = "gray", linewidth = 0.5))
    
    # Return value
    return(calendar.heatmap.ride.elevation)
  })
  #--------------------------------------
  # Output plot data as DT dataTable
  ## plot=lineplot.ride.distance
  ## plot=calendar.heatmap.ride.distance
  ## plot=lineplot.ride.elevation
  ## plot=calendar.heatmap.ride.elevation
  #--------------------------------------
  output$table.year.ride.max.distance.elevation.DT <- DT::renderDataTable({
    return(year.ride.max.distance.elevation())
  })
  
  output$table.lineplot.ride.distance.elevation.DT <- DT::renderDataTable({
    bike.ride.day() |> 
      dplyr::ungroup()|>
      dplyr::mutate(start.date.local.start.day.local= paste(start.date.local, start.day.local)
                    ,elevation.gain.m.day=round(elevation.gain.m.day, digits = 0)
                    ,ride.elevation.cum.year=round(ride.elevation.cum.year, digits = 0)) |>
      dplyr::select(start.date.local.start.day.local
                    ,start.dayofyear.local
                    ,activity.name.day
                    ,distance.km.day, ride.distance.cum.year
                    ,elevation.gain.m.day, ride.elevation.cum.year) |>
      dplyr::rename( Date=start.date.local.start.day.local
                     ,`Day of year`=start.dayofyear.local
                     ,Activities=activity.name.day
                     ,Distance=distance.km.day
                     ,`Distance running total`=ride.distance.cum.year
                     ,`Elevation gain`=elevation.gain.m.day
                     ,`Elevation running total`=ride.elevation.cum.year)
  }) # Close the renderDataTable function
  
  #*****************************************
  # Outputs to use for menuItem "Jobs"
  ## [R Shiny DT render text and input without linebreaks](https://stackoverflow.com/questions/72899453/r-shiny-dt-render-text-and-input-without-linebreaks)
  #*****************************************
  output$table.jobs.DT <- DT::renderDataTable({
    table.jobs.DT <- jobs |> 
      # Add a linebreak readable to Shiny with 2 steps: (1) add <br> as linebreak symbol, (2) set datatable(escape=FALSE)
      dplyr::mutate(compnay.position= paste(company,"<br>",position.title)) |>
      dplyr::select(compnay.position, event,event.start.date, event.end.date, event.duration.days) |>
      dplyr::rename( Position=compnay.position
                    ,Event=event
                    ,`Start date`=event.start.date
                    ,`End date`=event.end.date
                    ,`Duration (days)`= event.duration.days)
    DT::datatable(table.jobs.DT, escape = FALSE)
  }) # Close the renderDataTable function

} # Close the server function

#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#